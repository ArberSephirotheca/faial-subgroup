open Stage0
open Protocols
open Location_parser

type json = Yojson.Basic.t
type j_object = Rjson.j_object
type 'a j_result = 'a Rjson.j_result

let ( let* ) = Result.bind
let ( >>= ) = Result.bind

let parse_variable (j : json) : Variable.t j_result =
  (let open Rjson in
   let* o = cast_object j in
   let* name = with_field "name" cast_string o in
   match List.assoc_opt "range" o with
   | Some range ->
       let* l = parse_location range in
       let l =
         if Location.length l = 0 then
           Location.set_length (String.length name) l
         else l
       in
       Ok (Variable.make ~location:l ~name)
   | None -> Ok (Variable.from_name name))
  |> Rjson.add_reason "parse_variable" j

let is_invalid (o : j_object) : bool =
  let open Rjson in
  with_opt_field "isInvalid" cast_bool o
  |> Result.value ~default:None
  |> Option.value ~default:false

let expect_kind (k : string) (o : j_object) : unit j_result =
  let open Rjson in
  let* obtained : string = get_kind o in
  if obtained = k then Ok ()
  else
    root_cause
      ("Expecting kind '" ^ k ^ "' but got '" ^ obtained ^ "'")
      (`Assoc o)

let parse_attr (j : Yojson.Basic.t) : string j_result =
  let open Rjson in
  let* o = cast_object j in
  let* v = with_field "value" cast_string o in
  Ok (String.trim v)

let j_filter_kind (f : string -> bool) (j : Yojson.Basic.t) : bool =
  let open Rjson in
  let res =
    let* o = cast_object j in
    let* k = get_kind o in
    Ok (f k)
  in
  res |> Result.value ~default:false
