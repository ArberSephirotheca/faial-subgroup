(*
 Given a location-split kernel, generate a flat kernel.

 A flat-kernel has the following characteristics:
 - free from control-flow structures
 - all binders are hoisted
 - all concurrent accesses are available
 *)
open Stage0
open Protocols
open Exp

module CondAccess = struct
  type t = { access : Access.t; cond : bexp }

  let add_cond (b : bexp) (c : t) : t = { c with cond = b_and b c.cond }
  let dim (a : t) : int = List.length a.access.index
  let location (x : t) : Location.t = Access.location x.access
  let access (x : t) : Access.t = x.access

  let to_s (a : t) : Indent.t list =
    let lineno =
      a |> location |> Location.line |> Index.to_base1 |> string_of_int
    in
    let lineno = lineno ^ ": " in
    [
      Line (lineno ^ Access.to_string a.access ^ " if");
      Block (b_to_s a.cond);
      Line ";";
    ]

  let to_string (a : t) : string = to_s a |> Indent.to_string
end

module Code = struct
  type t = CondAccess.t list

  let to_list : t -> CondAccess.t list = fun x -> x
  let to_s (l : t) : Indent.t list = List.concat_map CondAccess.to_s l

  (* The dimention is the index count *)
  let dim (l : t) : int option = List.nth_opt l 0 |> Option.map CondAccess.dim

  let from_unsync : Unsync.t -> t =
    let rec flatten (accum : t) (b : bexp) : Unsync.t -> t = function
      | Skip -> accum
      | Assert _ ->
          failwith "Internall error: call Unsync.inline_asserts first!"
      | Access e -> { access = e; cond = b } :: accum
      | Cond (b', p) -> flatten accum (b_and b' b) p
      | Loop (r, p) -> flatten accum (b_and (Range.to_cond r) b) p
      | Seq (p, q) ->
          let accum = flatten accum b p in
          flatten accum b q
    in
    fun u -> flatten [] (Bool true) (Unsync.inline_asserts u)
end

module Kernel = struct
  type t = {
    name : string;
    array_name : string;
    approx_local_variables : Variable.Set.t;
    exact_local_variables : Variable.Set.t;
    code : Code.t;
    pre : bexp;
    runtime : bexp;
  }

  let to_s (k : t) : Indent.t list =
    [
      Line ("array: " ^ k.array_name ^ ";");
      Line
        ("exact locals: " ^ Variable.set_to_string k.exact_local_variables ^ ";");
      Line
        ("approx locals: "
        ^ Variable.set_to_string k.approx_local_variables
        ^ ";");
      Line ("pre: " ^ b_to_string k.pre ^ ";");
      Line ("rt: " ^ b_to_string k.runtime ^ ";");
      Line "{";
      Block (Code.to_s k.code);
      Line "}";
    ]

  let from_loc_split (arch : Architecture.t) (k : Locsplit.Kernel.t) : t option
      =
    let code = Code.from_unsync k.code in
    if code = [] then None
    else
      let ids =
        (match arch with
          | Grid -> Variable.bid_set
          | Block -> Variable.Set.empty)
        |> Variable.Set.union Variable.tid_set
      in
      let approx_local_variables =
        Variable.Set.diff (Params.to_set k.local_variables) ids
        |> Unsync.unsafe_binders k.code
      in
      let exact_local_variables =
        approx_local_variables
        |> Variable.Set.diff (Unsync.binders k.code Variable.Set.empty)
        |> Variable.Set.union ids
      in
      let pre = b_and_ex (List.map Range.to_cond k.ranges) in
      Some
        {
          name = k.name;
          array_name = k.array_name;
          code;
          exact_local_variables;
          approx_local_variables;
          pre;
          runtime =
            (* merge all parameters *)
            k.global_variables
            |> Params.union_left k.local_variables
            (* only retain the parameters that are used *)
            |> Params.retain_all (Locsplit.Kernel.free_names k)
            (* generate data type constraints *)
            |> Params.to_bexp;
        }
end

let translate (arch : Architecture.t)
    (stream : Locsplit.Kernel.t Streamutil.stream) : Kernel.t Streamutil.stream
    =
  let open Streamutil in
  filter_map (Kernel.from_loc_split arch) stream

(* ------------------- SERIALIZE ---------------------- *)

let print_kernels (ks : Kernel.t Streamutil.stream) : unit =
  print_endline "; flatacc";
  let count = ref 0 in
  Streamutil.iter
    (fun (k : Kernel.t) ->
      let curr = !count + 1 in
      count := curr;
      print_endline ("; acc " ^ string_of_int curr);
      Indent.print (Kernel.to_s k))
    ks;
  print_endline "; end of flatacc"
