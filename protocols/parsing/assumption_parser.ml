(* Parses the [--assume] clause syntax into a [Protocols.Assumption.t]:

     [<preamble>:] <bexp>

   [<preamble>] is a comma-separated list of [key=value] entries; each key is
   optional and may appear at most once:

     kernel=<name>   restrict the clause to a single kernel  (soft filter)
     binder=<name>   the binder the clause must attach to     (hard demand)
     line=<int>      source line disambiguating a reused binder label

   Examples:

     kernel=k_get_rows,binder=i00,line=16: i00 < ne00
     binder=nex_prev: nex_prev >= 0
     x < 10

   A [:] always separates the preamble from the [bexp]: the boolean-expression
   grammar has no [:] token, so the first [:] is unambiguous. With no [:] the
   whole string is the [bexp]. Because [line] lives under [binder] in the type,
   [line=] without [binder=] is a syntax error here. *)

open Protocols
module Match = Assumption.Match
module Target = Assumption.Target

(* Split [s] at the first [c]; the separator is dropped from both sides. *)
let split_first (c : char) (s : string) : (string * string) option =
  match String.index_opt s c with
  | None -> None
  | Some i ->
      Some (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))

let ( let* ) = Result.bind

(* The preamble fields, before assembling the target. *)
type raw = {
  kernel : string Match.t;
  binder : string option;
  line : int Match.t;
}

let raw0 : raw = { kernel = Match.Any; binder = None; line = Match.Any }

let add_entry (acc : raw) (part : string) : (raw, string) result =
  let part = String.trim part in
  match split_first '=' part with
  | None ->
      if part = "" then Error "empty preamble entry (stray comma)"
      else Error (Printf.sprintf "expected key=value, got %S" part)
  | Some (key, value) -> (
      let key = String.trim key and value = String.trim value in
      if value = "" then Error (Printf.sprintf "empty value for key %S" key)
      else
        match key with
        | "kernel" -> (
            match acc.kernel with
            | Match.Exact _ -> Error "duplicate key \"kernel\""
            | Match.Any -> Ok { acc with kernel = Match.Exact value })
        | "binder" -> (
            match acc.binder with
            | Some _ -> Error "duplicate key \"binder\""
            | None -> Ok { acc with binder = Some value })
        | "line" -> (
            match acc.line with
            | Match.Exact _ -> Error "duplicate key \"line\""
            | Match.Any -> (
                match int_of_string_opt value with
                | Some n when n >= 0 -> Ok { acc with line = Match.Exact n }
                | _ ->
                    Error
                      (Printf.sprintf "line must be a non-negative integer, got %S"
                         value)))
        | _ ->
            Error
              (Printf.sprintf "unknown key %S (expected kernel, binder, or line)"
                 key))

let build (raw : raw) (bexp : Exp.bexp) : (Assumption.t, string) result =
  match raw.binder with
  | Some label ->
      Ok { Assumption.kernel = raw.kernel; target = Target.Binder { label; line = raw.line }; bexp }
  | None -> (
      match raw.line with
      | Match.Exact _ -> Error "line= requires binder="
      | Match.Any -> Ok { Assumption.kernel = raw.kernel; target = Target.Pre; bexp })

let parse_bexp (s : string) : (Exp.bexp, string) result =
  let s = String.trim s in
  if s = "" then Error "missing boolean expression"
  else Parsers.BExpParser.of_string s

let of_string (input : string) : (Assumption.t, string) result =
  match split_first ':' input with
  | None ->
      let* bexp = parse_bexp input in
      build raw0 bexp
  | Some (pre, rest) ->
      if String.trim pre = "" then Error "empty preamble before ':'"
      else
        let* raw =
          String.split_on_char ',' pre
          |> List.fold_left
               (fun acc part ->
                 let* acc = acc in
                 add_entry acc part)
               (Ok raw0)
        in
        let* bexp = parse_bexp rest in
        build raw bexp
