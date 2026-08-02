open Stage0
open Protocols
module StringMap = Common.StringMap
module StringMapUtil = Common.StringMapUtil
module StringSet = Common.StringSet

type t =
  | Skip
  | Seq of t * t
  | Sync of Sync.t
  | Assert of Assert.t
  | Read of Read.t
  | Atomic of Atomic_write.t
  | Write of Write.t
  | LocationAlias of { target : Variable.t; pointer : Pointer.t }
  | Decl of Decl.t
  | Assign of { var : Variable.t; data : Exp.nexp; ty : Ty.t }
  | If of (Exp.bexp * t * t)
  | For of (Range.t * t)
  | Star of t
  | Call of Call.t

let filter (f : t -> bool) : t -> t =
  let rec filter (s : t) : t =
    if not (f s) then Skip
    else
      match s with
      | Seq (p, q) -> Seq (filter p, filter q)
      | If (b, p, q) -> If (b, filter p, filter q)
      | For (r, p) -> For (r, filter p)
      | Star p -> Star (filter p)
      | Sync _ | Read _ | Atomic _ | Write _ | LocationAlias _ | Decl _
      | Assign _ | Call _ | Assert _ | Skip ->
          s
  in
  filter

let filter_asserts (f : Assert.t -> bool) : t -> t =
  filter (function Assert a -> f a | _ -> true)

let is_for : t -> bool = function For _ -> true | _ -> false
let is_if : t -> bool = function If _ -> true | _ -> false

let rec has_sync : t -> bool = function
  | Sync _ -> true
  | Seq (p, q) | If (_, p, q) -> has_sync p || has_sync q
  | Atomic _ | Read _ | Write _ | Assert _ | LocationAlias _ | Decl _ | Call _
  | Assign _ | Skip ->
      false
  | For (_, p) | Star p -> has_sync p

let calls : t -> Function_id.Set.t =
  let rec calls (cs : Function_id.Set.t) : t -> Function_id.Set.t = function
    | Skip | Decl _ | LocationAlias _ | Sync _ | Assert _ | Read _ | Write _
    | Atomic _ | Assign _ ->
        cs
    | If (_, s1, s2) | Seq (s1, s2) -> calls (calls cs s1) s2
    | For (_, s) | Star s -> calls cs s
    | Call c -> Function_id.Set.add (Call.unique_id c) cs
  in
  calls Function_id.Set.empty

let fold : 'a. (t -> 'a -> 'a) -> t -> 'a -> 'a =
 fun (f : t -> 'a -> 'a) (p : t) (init : 'a) ->
  let rec fold_i (s : t) (init : 'a) : 'a =
    let init : 'a = f s init in
    match s with
    | Skip | Sync _ | Assert _ | Read _ | Atomic _ | Write _ | Decl _ | Assign _
    | LocationAlias _ | Call _ ->
        init
    | Seq (s1, s2) | If (_, s1, s2) ->
        let init = fold_i s1 init in
        fold_i s2 init
    | For (_, s) | Star s -> fold_i s init
  in
  fold_i p init

let find_all_map (f : t -> 'a option) (s : t) : 'a Seq.t =
  let g (e : t) (r : 'a Seq.t) : 'a Seq.t =
    match f e with Some x -> Seq.cons x r | None -> r
  in
  fold g s Seq.empty

let find_all (f : t -> bool) : t -> t Seq.t =
  find_all_map (fun x -> if f x then Some x else None)

let rec first : t -> t = function Seq (p, _) -> first p | s -> s

let for_ (r : Range.t) (s : t) : t =
  match s with Skip -> Skip | _ -> For (r, s)

let if_ (b : Exp.bexp) (p1 : t) (p2 : t) : t =
  match (b, p1, p2) with
  | Bool b, p, q -> if b then p else q
  | _, Skip, Skip -> Skip
  | _ -> If (b, p1, p2)

let seq (s1 : t) (s2 : t) : t =
  match (s1, s2) with Skip, s | s, Skip -> s | _, _ -> Seq (s1, s2)

let rec last : t -> t = function Seq (_, s) -> last s | s -> s

let rec skip_last : t -> t = function
  | Seq (s1, s2) -> seq s1 (skip_last s2)
  | _ -> Skip

let from_list : t list -> t = List.fold_left seq Skip
let decl (d : Decl.t) : t = Decl d
let decl_set (x : Variable.t) (e : Exp.nexp) : t = Decl (Decl.set x e)

(*
let decl_unset (x:Variable.t) : t =
  Decl (Decl.unset x)
*)
let to_list ?(rev = true) : t -> t list =
  let rec loop (accum : t list) : t -> t list = function
    | Skip -> accum
    | Seq (s1, s2) -> loop (loop accum s1) s2
    | ( Sync _ | Assert _ | Read _ | Write _ | LocationAlias _ | Atomic _
      | Decl _ | Assign _ | If _ | For _ | Star _ | Call _ ) as s ->
        s :: accum
  in
  fun s -> loop [] s |> if rev then List.rev else Fun.id

let assign (ty : Ty.t) (var : Variable.t) (data : Exp.nexp) : t =
  Assign { ty; var; data }

let decl_unset (v : Variable.t) : t = Decl (Decl.unset v)

let to_s : t -> Indent.t list =
  let rec stmt_to_s : t -> Indent.t list = function
    | Call c -> [ Line (Call.to_string c) ]
    | Sync s -> [ Line (Sync.to_string s ^ ";") ]
    | Assert b -> [ Line (Assert.to_string b ^ ";") ]
    | Atomic r ->
        [
          Line
            (Ty.to_string r.ty ^ " " ^ Variable.name r.target ^ " = atomic "
           ^ Variable.name r.array
            ^ Access.index_to_string r.index
            ^ ";");
        ]
    | Read r ->
        let a = Variable.name r.array in
        let idx = Access.index_to_string r.index in
        let prefix =
          match r.target with
          | Some (ty, target) ->
              let x = Variable.name target in
              let ty = Ty.to_string ty in
              ty ^ " " ^ x ^ " = "
          | None -> ""
        in
        [ Line (prefix ^ "rd " ^ a ^ idx ^ ";") ]
    | Write w ->
        let payload : string =
          match w.payload with None -> "" | Some x -> " = " ^ string_of_int x
        in
        [
          Line
            ("wr " ^ Variable.name w.array
            ^ Access.index_to_string w.index
            ^ payload ^ ";");
        ]
    | Skip -> [ Line "skip;" ]
    | Assign a ->
        [ Line (Variable.name a.var ^ " = " ^ Exp.n_to_string a.data ^ ";") ]
    | LocationAlias l ->
        [
          Line
            ("alias " ^ Variable.name l.target ^ " = "
            ^ Pointer.to_string l.pointer
            ^ ";"
            ^ Pointer.step_comment l.pointer);
        ]
    | Decl d -> [ Line ("decl " ^ Decl.to_string d ^ ";") ]
    | If (b, s1, Skip) ->
        [
          Line ("if (" ^ Exp.b_to_string b ^ ") {");
          Block (stmt_to_s s1);
          Line "}";
        ]
    | Seq (s1, s2) -> stmt_to_s s1 @ stmt_to_s s2
    | If (b, s1, s2) ->
        [
          Line ("if (" ^ Exp.b_to_string b ^ ") {");
          Block (stmt_to_s s1);
          Line "} else {";
          Block (stmt_to_s s2);
          Line "}";
        ]
    | Star s -> [ Line "foreach (?) {"; Block (stmt_to_s s); Line "}" ]
    | For (r, s) ->
        [
          Line ("foreach (" ^ Range.to_string r ^ ") {");
          Block (stmt_to_s s);
          Line "}";
        ]
  in
  stmt_to_s

let to_string (s : t) : string = Indent.to_string (to_s s)

module State = struct
  type 'a state = (t, 'a) Stage0.State.t

  let add (s : t) : unit state = Stage0.State.update (fun s' -> seq s' s)
  let run (m : unit state) : t = Stage0.State.run_update m Skip

  let try_run (m : 'a state) : 'a option =
    match Stage0.State.run m Skip with Skip, a -> Some a | _ -> None
end
