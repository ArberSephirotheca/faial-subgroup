open Stage0

type json = Yojson.Basic.t
type 'a j_result = 'a Rjson.j_result

let ( let* ) = Result.bind

(* clang's JSONNodeDumper emits implicit decls (e.g. [std::align_val_t])
   without a source location. Two shapes for "no location" appear in
   practice and we treat both as [Location.empty]:

     {}                                          (* legacy c-to-json *)
     {file: "<invalid>", line: 0, col: 0}        (* current c-to-json *)

   Returning a real [Location.t] (rather than an [Error]) lets the
   surrounding parsers continue with [let*] chains; downstream callers
   that need to distinguish "no location" can compare against
   [Location.empty]. *)
let is_invalid_position (o : (string * Yojson.Basic.t) list) : bool =
  match o with
  | [] -> true
  | _ ->
      let file =
        match List.assoc_opt "file" o with
        | Some (`String s) -> Some s
        | _ -> None
      in
      file = Some "<invalid>"

let rec parse_position ?(filename = "") : json -> Location.t j_result =
  let open Rjson in
  fun (j : json) ->
    let* o = cast_object j in
    if is_invalid_position o then Ok Location.empty
    else
      match
        let* line = with_field "line" cast_int o in
        let line = Index.from_base1 line in
        let* col = with_field "col" cast_int o in
        let interval = Index.from_base1 col |> Interval.from_start in
        let* filename : string = with_field_or "file" cast_string filename o in
        Ok (Location.make ~filename ~line ~interval)
      with
      | Ok p -> Ok p
      | Error _ -> with_field "expansionLoc" parse_position o

let parse_location (j : json) : Location.t j_result =
  let open Rjson in
  let open Location in
  let* o = cast_object j in
  let* first = with_field "begin" parse_position o in
  let last =
    o
    |> with_field "end" (parse_position ~filename:first.filename)
    |> Result.value ~default:first
  in
  Ok (Location.add_or_reset_lhs first last)
