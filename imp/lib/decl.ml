open Protocols

(* [pre] is an optional bexp that holds at this declaration. When the
   Decl is hoisted to kernel locals by [Protocols.Kernel.hoist_decls],
   the [pre] is conjoined into [Kernel.pre] so it becomes a global
   hypothesis on every subsequent access. Example: an [atomicAdd]
   target with a positive literal increment carries [pre = Some
   (b_not (thread_eq (Var target)))], encoding that distinct threads
   see distinct return values. [None] for declarations that carry
   no extra invariant. *)
type t = {
  var : Variable.t;
  ty : C_type.t;
  init : Exp.nexp option;
  pre : Exp.bexp option;
}

let set ?(ty = C_type.int) ?(pre = None) (var : Variable.t)
    (init : Exp.nexp) : t =
  { init = Some init; ty; var; pre }

let unset ?(ty = C_type.int) ?(pre = None) (var : Variable.t) : t =
  { init = None; ty; var; pre }

let map (f : Exp.nexp -> Exp.nexp) (d : t) : t =
  { d with init = Option.map f d.init }

let from_set (vs : Variable.Set.t) : t list =
  vs |> Variable.Set.elements |> List.map (fun v -> unset v)

let to_string (d : t) : string =
  let ty = C_type.to_string d.ty in
  let x = Variable.name d.var in
  let init =
    d.init
    |> Option.map (fun n -> " = " ^ Exp.n_to_string n)
    |> Option.value ~default:""
  in
  let pre =
    d.pre
    |> Option.map (fun b -> " pre: " ^ Exp.b_to_string b)
    |> Option.value ~default:""
  in
  let label =
    match Variable.label_opt d.var with
    | Some l -> " /* " ^ l ^ " */"
    | None -> ""
  in
  ty ^ " " ^ x ^ init ^ pre ^ label
