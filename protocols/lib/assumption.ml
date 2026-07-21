(* An assumption attaches a boolean fact to a kernel: either the kernel
   precondition, or a specific binder (a loop counter or a declaration).

   The two selectors have opposite failure modes, and the type says so:

   - [kernel] is a soft filter. [Any] applies to every kernel; [Exact n] to
     kernel [n]. A kernel that does not match is left untouched.
   - [target] is the destination. [Pre] conjoins the fact into the kernel
     precondition. [Binder] is a hard demand: it must resolve to exactly one
     binder, otherwise adding the assumption fails.

   [line] disambiguates a binder whose label is reused across scopes. It lives
   inside [MatchBinder], so a line without a binder to disambiguate cannot be
   represented. [Any] resolves by label alone (and must be unique); [Exact n]
   pins the binder introduced at source line [n].

   The concrete syntax and its parser live in [protocols/parsing]; this module
   holds the type and the two operations that apply it. *)

open Stage0

module Match = struct
  type 'a t = Exact of 'a | Any
end

module MatchBinder = struct
  type t = { label : string; line : int Match.t }
end

module Target = struct
  type t = Pre | Binder of MatchBinder.t
end

type t = {
  kernel : string Match.t;
  target : Target.t;
  bexp : Exp.bexp;
}

(* One-based source line where [v] is introduced, when known. *)
let binder_line (v : Variable.t) : int option =
  Variable.location_opt v |> Option.map (fun l -> Index.to_base1 (Location.line l))

let loc_to_string (v : Variable.t) : string =
  match binder_line v with
  | Some n -> "line " ^ string_of_int n
  | None -> "unknown line"

(* A binder variable [v] matches [mb] when its label agrees and, if a line is
   pinned, its source line agrees. Matching is on the label (the source name),
   not the internal name, so it survives [vars_distinct] renaming. *)
let matches (mb : MatchBinder.t) (v : Variable.t) : bool =
  String.equal (Variable.label v) mb.label
  &&
  match mb.line with
  | Match.Any -> true
  | Match.Exact n -> binder_line v = Some n

let to_string (a : t) : string =
  let of_match render = function Match.Any -> [] | Match.Exact x -> [ render x ] in
  let kernel = of_match (fun n -> "kernel=" ^ n) a.kernel in
  let target =
    match a.target with
    | Target.Pre -> []
    | Target.Binder mb ->
        ("binder=" ^ mb.label)
        :: of_match (fun n -> "line=" ^ string_of_int n) mb.line
  in
  let preamble = kernel @ target in
  (if preamble = [] then "" else String.concat "," preamble ^ ": ")
  ^ Exp.b_to_string a.bexp

(* Conjoin [a.bexp] onto every binder matching [a.target], returning the
   rewritten code and the binders hit. For [Pre] there is no binder, so the
   code is returned unchanged with an empty list. This does not enforce "exactly
   one" -- that is policy, decided in [add_to_kernel]. *)
let add_to_code (a : t) (code : Code.t) : Code.t * Variable.t list =
  match a.target with
  | Target.Pre -> (code, [])
  | Target.Binder mb ->
      let rec walk (code : Code.t) : Code.t * Variable.t list =
        match code with
        | Code.Skip | Code.Access _ | Code.Sync _ -> (code, [])
        | Code.If (b, p, q) ->
            let p, hp = walk p in
            let q, hq = walk q in
            (Code.If (b, p, q), List.append hp hq)
        | Code.Seq (p, q) ->
            let p, hp = walk p in
            let q, hq = walk q in
            (Code.Seq (p, q), List.append hp hq)
        | Code.Decl d ->
            let body, hits = walk d.body in
            if matches mb d.var then
              (Code.Decl { d with cond = Exp.b_and d.cond a.bexp; body }, d.var :: hits)
            else (Code.Decl { d with body }, hits)
        | Code.Loop { cond_range; body } ->
            let body, hits = walk body in
            let v = Cond_range.var cond_range in
            if matches mb v then
              let cond_range =
                Cond_range.make cond_range.range (Exp.b_and cond_range.cond a.bexp)
              in
              (Code.Loop { cond_range; body }, v :: hits)
            else (Code.Loop { cond_range; body }, hits)
      in
      walk code

let ( let* ) = Result.bind

(* Names used in [k] that are bound by neither a parameter, a launch-config
   built-in, nor an enclosing binder. A well-formed kernel has none, so the
   difference before and after adding an assumption is exactly the names that
   assumption introduced. *)
let unbound_names (k : Kernel.t) : Variable.Set.t =
  Kernel.free_names k
  |> Variable.Set.filter (fun v -> not (Variable.is_launch_config v))

(* Apply [a] to [k]. A kernel filtered out by [a.kernel] is returned unchanged.
   [Pre] conjoins into the precondition; [Binder] must resolve to exactly one
   binder. Finally, strict well-formedness: the assumption may not leave any
   name unbound (a mistyped variable, or a binder reference out of scope, which
   includes a binder whose internal name no longer matches the source label);
   otherwise it is rejected. *)
let add_to_kernel (a : t) (k : Kernel.t) : (Kernel.t, string) result =
  let applies =
    match a.kernel with
    | Match.Any -> true
    | Match.Exact n -> String.equal n (Kernel.name k)
  in
  if not applies then Ok k
  else
    let* k' =
      match a.target with
      | Target.Pre -> Ok (Kernel.add_pre a.bexp k)
      | Target.Binder mb -> (
          let code, hits = add_to_code a k.code in
          match hits with
          | [ _ ] -> Ok { k with code }
          | [] ->
              let where =
                match mb.line with
                | Match.Exact n -> Printf.sprintf " at line %d" n
                | Match.Any -> ""
              in
              Error
                (Printf.sprintf "no binder named %S%s in kernel %S" mb.label
                   where (Kernel.name k))
          | _ ->
              Error
                (Printf.sprintf
                   "ambiguous binder %S in kernel %S: bound at %s; add line= to \
                    disambiguate"
                   mb.label (Kernel.name k)
                   (String.concat ", " (List.map loc_to_string hits))))
    in
    let introduced = Variable.Set.diff (unbound_names k') (unbound_names k) in
    if Variable.Set.is_empty introduced then Ok k'
    else
      Error
        (Printf.sprintf "assumption introduces unbound name(s) %s in kernel %S"
           (Variable.set_to_string introduced) (Kernel.name k))
