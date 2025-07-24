open Stage0
open Protocols
open Exp
module Opt = Unsync.Opt

module Code = struct
  type t = SInst of Sync.t | UInst of Unsync.t | Both of Sync.t * Unsync.t

  let add_u (u : Unsync.t) : t -> t = function
    | SInst s -> SInst (Sync.add u s)
    | UInst u2 -> UInst (Opt.seq u u2)
    | Both (p, u2) -> Both (Sync.add u p, u2)

  let add_s (s : Sync.t) : t -> t = function
    | SInst s2 -> SInst (Seq (s, s2))
    | UInst u -> Both (s, u)
    | Both (s2, u) -> Both (Seq (s, s2), u)

  let seq (p : t) (q : t) : t =
    match (p, q) with
    | UInst u, s -> add_u u s
    | SInst p, s -> add_s p s
    | Both (p, u), s -> add_s p (add_u u s)

  let free_names (p : t) (fns : Variable.Set.t) : Variable.Set.t =
    match p with
    | SInst s -> Sync.free_names s fns
    | UInst s -> Unsync.free_names s fns
    | Both (p, q) -> Sync.free_names p fns |> Unsync.free_names q

  (* Given a regular program, return a well-formed one *)
  let from_proto : Protocols.Code.t -> Sync.t Streamutil.stream =
    let open Streamutil in
    let rec infer : Protocols.Code.t -> t Streamutil.stream = function
      | Skip -> UInst Skip |> one
      | Access a -> UInst (Access a) |> one
      | Sync _ -> SInst Sync.skip |> one
      | Decl _ -> failwith "Invoke Proto.hoist_decls first."
      | If (b, p, q) ->
          let branch b p =
            infer p
            |> flat_map (function
                 (* TODO: why should we reject synchronized conditionals inside loops? *)
                 | SInst p ->
                     [ UInst (Assert (b_not b)); SInst (Sync.inline_cond b p) ]
                     |> from_list
                 | Both (p, c) ->
                     [
                       UInst (Assert (b_not b));
                       Both (Sync.inline_cond b p, Opt.seq (Assert b) c);
                     ]
                     |> from_list
                 | UInst c -> UInst (Opt.cond b c) |> one)
          in
          branch b p
          |> flat_map (fun p -> branch (Exp.b_not b) q |> map (seq p))
      | Loop { range; body = p } ->
          infer p
          |> map (function
               | Both (p, c) -> SInst (SeqLoop (Skip, { range; body = (p, c) }))
               | SInst p -> SInst (SeqLoop (Skip, { range; body = (p, Skip) }))
               | UInst c -> UInst (Opt.loop range c))
      | Seq (p, q) -> infer p |> flat_map (fun p -> infer q |> map (seq p))
    in
    fun p ->
      infer p
      |> map (function
           | SInst p -> p
           | UInst c -> Sync c
           | Both (p, c) -> Sync.Seq (p, Sync.Sync c))
end

module Kernel = struct
  type t = {
    (* The kernel name *)
    name : string;
    (* The internal variables are used in the code of the kernel.  *)
    global_variables : Params.t;
    (* The internal variables are used in the code of the kernel.  *)
    local_variables : Params.t;
    (* The modifiers of each array *)
    arrays : Memory.t Variable.Map.t;
    (* A thread-local pre-condition that is true on all phases. *)
    pre : bexp;
    (* The code of a kernel performs the actual memory accesses. *)
    code : Sync.t;
    (* The kernel's visibility *)
    visibility : Visibility.t;
    (* Number of blocks *)
    grid_dim : Dim3.t option;
    (* Number of blocks *)
    block_dim : Dim3.t option;
  }

  let to_s (k : t) : Indent.t list =
    [
      Line ("name: " ^ k.name ^ ";");
      Line ("arrays: " ^ Memory.map_to_string k.arrays ^ ";");
      Line ("globals: " ^ Params.to_string k.global_variables ^ ";");
      Line ("locals: " ^ Params.to_string k.local_variables ^ ";");
      Line "invariant:";
      Block (b_to_s k.pre);
      Line ";";
      Line "code:";
      Block (Sync.to_s k.code);
      Line "; end of code";
    ]

  let print (k : t) : unit = Indent.print (to_s k)

  let binders (k : t) : Variable.Set.t =
    Sync.free_names k.code Variable.Set.empty |> Exp.b_free_names k.pre

  let trim_binders (k : t) : t =
    let fns = binders k in
    {
      k with
      global_variables = Params.retain_all fns k.global_variables;
      local_variables = Params.retain_all fns k.local_variables;
    }

  let from_protocol (k : Protocols.Kernel.t) : t Streamutil.stream =
    let k = Protocols.Kernel.hoist_decls k in

    let from_protocol (code : Sync.t) : t =
      {
        name = k.name;
        global_variables = k.global_variables;
        local_variables = k.local_variables;
        arrays = k.arrays;
        pre = k.pre;
        code;
        visibility = k.visibility;
        grid_dim = k.grid_dim;
        block_dim = k.block_dim;
      }
    in
    Code.from_proto k.code |> Streamutil.map from_protocol
end

let translate : Protocols.Kernel.t -> Kernel.t Streamutil.stream =
  Kernel.from_protocol

let print_kernels (ks : Kernel.t Streamutil.stream) : unit =
  print_endline "; w-lang";
  let count = ref 0 in
  Streamutil.iter
    (fun k ->
      let curr = !count + 1 in
      count := curr;
      print_endline ("; w-prog " ^ string_of_int curr);
      Kernel.print k)
    ks;
  print_endline "; end of w-lang"
