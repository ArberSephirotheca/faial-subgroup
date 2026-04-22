open Stage0
open Protocols
open Subst

(** This code is syntactically limited so that PreCode.seq is a total function
*)
module PreCode = struct
  type t = Sync of Unsynced.t | SeqLoop of t * loop | Seq of t * t
  and loop = { range : Range.t; body : t }

  module Make (S : SUBST) = struct
    module M = Subst.Make (S)
    module U = Unsynced.Make (S)

    let rec subst (s : S.t) (i : t) : t =
      match i with
      | Seq (p, q) -> Seq (subst s p, subst s q)
      | Sync c -> Sync (U.subst s c)
      | SeqLoop (p, { range = r; body = q }) ->
          let q = M.add s r.var (function Some s -> subst s q | None -> q) in
          SeqLoop (subst s p, { range = M.r_subst s r; body = q })
  end

  module S1 = Make (SubstPair)

  let subst = S1.subst

  let rec seq (c : Unsynced.t) : t -> t = function
    | Sync c' -> Sync (Unsynced.seq c c')
    | SeqLoop (p, l) -> SeqLoop (seq c p, l)
    | Seq (p, q) -> Seq (seq c p, q)

  let rec align : Synced.t -> t * Unsynced.t = function
    | Sync c -> (Sync c, Skip)
    | SeqLoop (before_loop, { range = r; body = s_body, u_body_post }) ->
        (* Rec yields the aligned body and leak *)
        let a_body, u_body_pre = align s_body in
        (* The unsynchronized body *)
        let u_body = Unsynced.seq u_body_pre u_body_post in
        (* First iteration: before_loop; aligned code (instantiated to first iter) *)
        let first_iter =
          seq before_loop (subst (r.var, Range.first r) a_body)
        in
        (* the leak of the previous iteration followed by the aligned *)
        let body = seq (Unsynced.subst (r.var, Range.prev r) u_body) a_body in
        ( SeqLoop (first_iter, { range = Range.next r; body }),
          (* leaks the last iteration *)
          Unsynced.subst (r.var, Range.lossy_last r) u_body )
    | Seq (i, p) ->
        let i, c1 = align i in
        let q, c2 = align p in
        (Seq (i, seq c1 q), c2)

  let align_ex (w : Synced.t) : t = match align w with p, c -> Seq (Sync c, p)
end

(** We first translate using PreCode and then we simplify the data-type. *)
module Code : sig
  type t =
    | Sync of Unsynced.t
    | Loop of { range : Range.t; body : t }
    | Seq of t * t

  val to_s : t -> Indent.t list
  val from_sync : Synced.t -> t
end = struct
  type t =
    | Sync of Unsynced.t
    | Loop of { range : Range.t; body : t }
    | Seq of t * t

  let rec to_s : t -> Indent.t list = function
    | Seq (p, q) -> to_s p @ to_s q
    | Sync e -> Unsynced.to_s e @ [ Line "sync;" ]
    | Loop { range = r; body = q } ->
        [
          Line ("foreach* (" ^ Range.to_string r ^ ") {");
          Block (to_s q);
          Line "}";
        ]

  let rec from_pre : PreCode.t -> t = function
    | Sync u -> Sync u
    | SeqLoop (p, { range; body }) ->
        Seq (from_pre p, Loop { range; body = from_pre body })
    | Seq (p, q) -> Seq (from_pre p, from_pre q)

  (* Load a protocol without side effects *)
  let rec from_pure : Synced.t -> t option =
    let ( let* ) = Option.bind in
    function
    | Sync u -> Some (Sync u)
    | SeqLoop (Skip, { range; body = body, Skip }) ->
        let* body = from_pure body in
        Some (Loop { range; body })
    | SeqLoop _ -> None
    | Seq (p, q) ->
        let* p = from_pure p in
        let* q = from_pure q in
        Some (Seq (p, q))

  (** Parse a pure protocol, or align a side-effect free protocol *)
  let try_align (s : Synced.t) : t option =
    match from_pure s with
    | Some s -> Some s
    | None ->
        let s, u = PreCode.align s in
        if u = Skip then Some (from_pre s) else None

  (** Forcefully align a protocol *)
  let align (s : Synced.t) : t = PreCode.align_ex s |> from_pre

  (** Try identify already aligned loops and if possible avoid aligning
      protocols. This should improve the performance of already synchronized
      protocls. *)
  let rec from_sync (s : Synced.t) : t =
    (*
      We try to infer pure syncs from left-to-right.
      Once we fail the first sync, we fall back to aligning the whole
      protocol.
     *)
    match s with
    | Sync u ->
        Sync u (* peel nested sequences, so that we can improve our coverage *)
    | Seq (Seq (p, q), r) -> from_sync (Seq (p, Seq (q, r)))
    | Seq (p, q) as seq -> (
        match try_align p with
        | Some p ->
            (* The first fragment is side-effect free, so we can digest it,
           and continue processing q without concerns about sid effects. *)
            Seq (p, from_sync q)
        | None ->
            (* once the first sub-protocol fails, align the whole sequence *)
            align seq)
    | SeqLoop (Skip, _) as p -> (
        (* Check if the loop body has no side-effects *)
        match from_pure p with
        | Some p ->
            (* no side effects, we can return it as is *)
            p
        | None ->
            (* has side effects, just align the whole protocol *)
            align p)
    | SeqLoop _ as p -> align p
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
    pre : Exp.bexp;
    (* The code of a kernel performs the actual memory accesses. *)
    code : Code.t;
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
      Block (Exp.b_to_s k.pre);
      Line ";";
      Line "code:";
      Block (Code.to_s k.code);
      Line "; end of code";
    ]

  let print (k : t) : unit = Indent.print (to_s k)

  let from_wellformed (k : Wellformed.Kernel.t) : t =
    {
      name = k.name;
      global_variables = k.global_variables;
      local_variables = k.local_variables;
      arrays = k.arrays;
      pre = k.pre;
      code = Code.from_sync k.code;
      visibility = k.visibility;
      grid_dim = k.grid_dim;
      block_dim = k.block_dim;
    }
end

let translate (ks : Wellformed.Kernel.t Streamutil.stream) :
    Kernel.t Streamutil.stream =
  Streamutil.map Kernel.from_wellformed ks

let print_kernels (ks : Kernel.t Streamutil.stream) : unit =
  print_endline "; a-lang";
  let count = ref 0 in
  Streamutil.iter
    (fun k ->
      let curr = !count + 1 in
      count := curr;
      print_endline ("; a-prog " ^ string_of_int curr);
      Kernel.print k)
    ks;
  print_endline "; end of a-lang"
