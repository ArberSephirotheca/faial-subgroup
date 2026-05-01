open Stage0

let split_array_type (x : string) : (string * string) option =
  match Common.split '[' x with
  | Some (x, y) ->
      let x = String.trim x in
      Some (x, "[" ^ y)
  | None -> (
      match Common.rsplit ' ' x with
      | Some (_, "*") as o -> o
      | Some (e, "*__restrict") -> Some (e, "*")
      | Some (e, "*const") -> Some (e, "*")
      | _ -> None)

let parse_dim (x : string) : int list =
  (*
    let ex3 = "[8][8]" in
    assert (parse_dim ex3 = Some [8; 8]);
  *)
  String.split_on_char '[' x
  |> List.concat_map (fun (x : string) ->
      if String.length x = 0 then []
      else [ String.sub x 0 (String.length x - 1) ])
  |> List.map int_of_string

let parse_array_type_opt (x : string) : string list option =
  match split_array_type x with
  | Some (x, _) ->
      Some
        (String.split_on_char ' ' x
        |> List.filter (fun x -> String.length x > 0))
  | None -> None

let parse_array_dim_opt (x : string) : int list option =
  match split_array_type x with
  | Some (_, x) -> (
      let x =
        match Common.rsplit ' ' x with
        | Some (x, "*") -> x
        | Some (x, "*__restrict") -> x
        | _ -> x
      in
      try Some (parse_dim x) with Failure _ -> None)
  | None -> None

(* ----------------------------------- *)

(* Type-safe representation of a CType *)
type t = CType : string -> t

let make (ty : string) : t = CType ty
let char : t = make "char"
let int : t = make "int"
let unsigned_int : t = make "unsigned int"
let unknown : t = make "?"
let to_string (c : t) : string = match c with CType x -> x

let is_pointer (c : t) =
  let c = to_string c in
  String.ends_with ~suffix:" *" c || String.ends_with ~suffix:" *__restrict" c

let is_function (c : t) : bool = Common.contains ~substring:"(*)" (to_string c)
let is_void (c : t) = to_string c = "void"
let is_auto (c : t) = to_string c = "auto"

let is_struct (c : t) : bool =
  String.starts_with ~prefix:"struct " (to_string c)

let get_array_length (c : t) : int list =
  to_string c |> parse_array_dim_opt |> Option.value ~default:[]

let get_array_type (c : t) : string list =
  to_string c |> parse_array_type_opt |> Option.value ~default:[]

let is_const (c : t) : bool = to_string c |> String.starts_with ~prefix:"const "

let strip_const (c : t) : t =
  let s = to_string c in
  if String.starts_with ~prefix:"const " s then
    CType (Slice.from_start (String.length "const ") |> Slice.substring s)
  else c

let is_array (c : t) : bool =
  to_string c |> parse_array_type_opt |> Option.is_some

let array_elements (c : t) : t option =
  split_array_type (to_string c) |> Option.map fst |> Option.map make

let strip_array (c : t) : t = array_elements c |> Option.value ~default:c

let sizeof (x : t) : int option =
  let x = to_string x in
  if String.ends_with ~suffix:"*" x then Some 8
  else
    let x =
      x |> String.split_on_char ' '
      |> List.filter (fun x -> x <> "const" || x <> "unsigned")
      |> String.concat " "
    in
    if String.starts_with ~prefix:"long" x then Some 8
    else if x = "int" then Some 4
    else if x = "short" then Some 2
    else if x = "char" then Some 1
    else if x = "float" then Some 4
    else if x = "double" then Some 8
    else None

let to_int_dom (c : t) : Int_dom.t option =
  let c = to_string c in
  let c =
    if String.starts_with ~prefix:"const " c then
      Slice.from_start (String.length "const ") |> Slice.substring c
    else c
  in
  match c with
  | "bool" | "char" | "signed char" | "int8_t" -> Some Int_dom.signed_char
  | "unsigned char" | "uchar" | "uint8_t" -> Some Int_dom.unsigned_char
  | "short" | "signed shot" | "int16_t" -> Some Int_dom.signed_short
  | "unsigned short" | "ushort" | "uint16_t" -> Some Int_dom.unsigned_short
  | "int" | "signed int" | "int32_t" -> Some Int_dom.signed_int
  | "unsigned int" | "uint" | "uint32_t" -> Some Int_dom.unsigned_int
  | "long" | "signed long" | "int64_t" -> Some Int_dom.signed_long
  | "unsigned long" | "ulong" | "size_t" | "uint64_t" ->
      Some Int_dom.unsigned_long
  | _ -> None

let is_int (c : t) : bool = to_int_dom c |> Option.is_some
