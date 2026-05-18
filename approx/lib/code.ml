open Protocols
open Stage0

let typecheck_n (env : Variable.Set.t) (n : Exp.nexp) : bool =
  Variable.Set.subset (Exp.n_free_names n Variable.Set.empty) env

let typecheck_b (env : Variable.Set.t) (b : Exp.bexp) : bool =
  Variable.Set.subset (Exp.b_free_names b Variable.Set.empty) env

let typecheck_a (env : Variable.Set.t) (a : Access.t) : bool =
  Variable.Set.subset (Access.free_names a Variable.Set.empty) env

let typecheck_r (env : Variable.Set.t) (r : Range.t) : bool =
  Variable.Set.subset (Range.free_names r Variable.Set.empty) env

type t =
  | Cond of Exp.bexp * t
  | Decl of { var : Variable.t; ty : C_type.t; body : t }
  | Loop of { range : Range.t; body : t }
  | Access of Access.t

let decl ?(ty = C_type.int) (var : Variable.t) (body : t) : t =
  Decl { ty; var; body }

let rec location : t -> Location.t = function
  | Cond (_, l) | Decl { body = l; _ } | Loop { body = l; _ } -> location l
  | Access { array = x; _ } -> Variable.location x

let rec from_code : Protocols.Code.t -> t Seq.t = function
  | Access a -> Seq.return (Access a)
  | Sync _ | Skip -> Seq.empty
  | If (b, p, q) ->
      Seq.append
        (from_code p |> Seq.map (fun p -> Cond (b, p)))
        (from_code q |> Seq.map (fun q -> Cond (Exp.b_not b, q)))
  | Loop { range = r; body = p } ->
      from_code p |> Seq.map (fun c -> Loop { range = r; body = c })
  | Seq (p, q) -> from_code p |> Seq.append (from_code q)
  (* Approx ignores the Protocols.Code.Decl's [pre] field: this IR
     captures over-approximations of access patterns, not the precise
     hypothesis pool that the race-detector consults. *)
  | Decl { var; ty; body } ->
      from_code body |> Seq.map (fun body -> Decl { var; ty; body })

let from_kernel (k : Protocols.Kernel.t) : t Seq.t =
  If (k.pre, k.code, Skip) |> from_code

let rec is_control_independent (env : Variable.Set.t) : t -> bool = function
  | Cond (b, p) -> typecheck_b env b && is_control_independent env p
  | Decl { body = p; var = x; _ } ->
      is_control_independent (Variable.Set.remove x env) p
  | Loop { range = r; body = p } ->
      typecheck_r env r
      && is_control_independent (Variable.Set.add (Range.var r) env) p
  | Access _ -> true

let rec is_data_independent (env : Variable.Set.t) : t -> bool = function
  | Cond (_, p) -> is_data_independent env p
  | Decl { var = x; body = p; _ } ->
      is_data_independent (Variable.Set.remove x env) p
  | Loop { range = r; body = p } ->
      let env =
        if typecheck_r env r then
          (* if range is independent, then loop var is independent *)
          Variable.Set.add (Range.var r) env
        else env
      in
      is_data_independent env p
  | Access a -> typecheck_a env a
