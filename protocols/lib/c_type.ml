open Stage0

(* C/C++ qualifiers that may follow the [*] in a pointer type:
   [const], [volatile], [restrict] (C99), and the GCC/Clang variants
   [__restrict] / [__restrict__]. None of these change the
   pointer-vs-not classification — they're orthogonal properties.

   c-to-json's qualType strings stack these in declaration order, so
   a parameter declared [volatile T* const __restrict] reaches us as
   ["volatile T *const __restrict"]; the suffix is variable. Strip
   recognised qualifiers off the tail until the bare [... *] remains
   so downstream string matching only has to handle the canonical
   form. *)
let pointer_qualifiers : string list =
  [ "__restrict__"; "__restrict"; "restrict"; "volatile"; "const" ]

let strip_pointer_quals (s : string) : string =
  let rec loop s =
    let s = String.trim s in
    let s' =
      List.fold_left
        (fun acc q ->
          if String.ends_with ~suffix:q acc then
            String.sub acc 0 (String.length acc - String.length q)
          else acc)
        s pointer_qualifiers
    in
    if s = s' then s else loop s'
  in
  loop s

let split_array_type (x : string) : (string * string) option =
  match Common.split '[' x with
  | Some (x, y) ->
      let x = String.trim x in
      Some (x, "[" ^ y)
  | None ->
      let stripped = strip_pointer_quals x in
      if String.ends_with ~suffix:" *" stripped then
        let e =
          String.sub stripped 0 (String.length stripped - 2) |> String.trim
        in
        Some (e, "*")
      else None

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
  let s = to_string c |> strip_pointer_quals in
  String.ends_with ~suffix:" *" s

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
  (* C [long long] / [signed long long] is a separate type from [long]
     in standard C but has the same 64-bit width on every platform
     faial targets. cu-to-json emits the type string verbatim
     (the literal long-long type string, two tokens) when the source
     uses that keyword (no desugar to long), so without this arm
     [is_int] returns false and [d_to_imp]'s [infer_decl] silently
     drops every long-long local, breaking dataflow chains that
     pass through such variables. Map to [signed_long] /
     [unsigned_long]: they share the same [Int_dom.t] (Bit64 size,
     current 32-bit-range clamp in [to_range]) so long-long values
     are over-approximated to the same range as long, conservative
     on race-detection. *)
  | "long long" | "signed long long" -> Some Int_dom.signed_long
  | "unsigned long long" -> Some Int_dom.unsigned_long
  | _ -> None

let is_int (c : t) : bool = to_int_dom c |> Option.is_some

let is_unsigned (c : t) : bool =
  match to_int_dom c with
  | Some d -> not d.signed
  | None -> false
