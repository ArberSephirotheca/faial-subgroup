type t = { name : string option; qualifiers : Qualifier.Set.t; inner : inner }

and inner =
  | Scalar of Scalar.t
  | Vector of { size : Vector_size.t; scalar : Scalar.t }
  | Matrix of {
      columns : Vector_size.t;
      rows : Vector_size.t;
      scalar : Scalar.t;
    }
  | Atomic of Scalar.t
  | Array of { base : t; size : int option }
  | Pointer of t
  (* A reference names the referent's storage rather than a value of its
     own, so it is not a pointer: nothing indexes through it. It is kept
     distinct so a const reference, which cannot be assigned through, can
     be read as its referent while a mutable one stays visibly unmodelled. *)
  | Reference of t
  | Struct of { members : (string * t) list }
  | Function
  | Void
  | Opaque of string

let make ?name ?(qualifiers = Qualifier.Set.empty) (inner : inner) : t =
  { name; qualifiers; inner }

let scalar (s : Scalar.t) : t = make (Scalar s)
let char : t = scalar Scalar.char
let int : t = scalar Scalar.int
let unsigned_int : t = scalar Scalar.unsigned_int
let void : t = make Void
let unknown : t = make (Opaque "?")
let opaque (s : string) : t = make (Opaque s)

(* ------------------------------ printing ------------------------------ *)

let rec inner_to_string : inner -> string = function
  | Scalar s -> Scalar.to_string s
  | Vector v ->
      "vec" ^ Vector_size.to_string v.size ^ "<" ^ Scalar.to_string v.scalar
      ^ ">"
  | Matrix m ->
      "mat"
      ^ Vector_size.to_string m.rows
      ^ "x"
      ^ Vector_size.to_string m.columns
      ^ "<" ^ Scalar.to_string m.scalar ^ ">"
  | Atomic s -> "atomic<" ^ Scalar.to_string s ^ ">"
  | Array a ->
      let size =
        match a.size with Some n -> string_of_int n | None -> ""
      in
      to_string a.base ^ "[" ^ size ^ "]"
  | Pointer p -> to_string p ^ " *"
  | Reference p -> to_string p ^ " &"
  | Struct { members = [] } -> "struct"
  | Struct { members } ->
      "struct {"
      ^ (members
        |> List.map (fun (n, ty) -> to_string ty ^ " " ^ n)
        |> String.concat ", ")
      ^ "}"
  | Function -> "(*)"
  | Void -> "void"
  | Opaque s -> s

and to_string (x : t) : string =
  match x.name with
  | Some n -> n
  | None ->
      (x.qualifiers |> Qualifier.Set.elements |> List.map Qualifier.to_string)
      @ [ inner_to_string x.inner ]
      |> String.concat " "

let equal (x : t) (y : t) : bool =
  x.name = y.name
  && Qualifier.Set.equal x.qualifiers y.qualifiers
  && x.inner = y.inner

(* ------------------------------- queries ------------------------------ *)

let is_pointer (x : t) : bool =
  match x.inner with Pointer _ -> true | _ -> false

let is_array (x : t) : bool = match x.inner with Array _ -> true | _ -> false

(* Every consumer that asks whether a name denotes memory it can index
   wants a pointer parameter to answer yes, which is what the string
   representation did by falling through to a [" *"] suffix test. *)
let is_array_or_pointer (x : t) : bool =
  match x.inner with Array _ | Pointer _ -> true | _ -> false

let is_struct (x : t) : bool =
  match x.inner with Struct _ -> true | _ -> false

let is_function (x : t) : bool = x.inner = Function
let is_void (x : t) : bool = x.inner = Void
let is_auto (x : t) : bool = x.inner = Opaque "auto"
let is_unknown (x : t) : bool = x.inner = Opaque "?"

let to_opaque (x : t) : string option =
  match x.inner with Opaque s -> Some s | _ -> None

(* Clang writes a leading [const] on the element of an array and on the
   pointee of a pointer, so the written spelling that [Ty.is_const]
   tested for is the qualifier reached by walking those two arms. *)
let rec is_const (x : t) : bool =
  Qualifier.Set.mem Const x.qualifiers
  ||
  match x.inner with
  | Array a -> is_const a.base
  | Pointer p -> is_const p
  | Reference p -> is_const p
  | _ -> false

let is_reference (x : t) : bool =
  match x.inner with Reference _ -> true | _ -> false

(* Storage a callee can assign through. The qualifier that decides it is
   the one on the referent, not the one on the pointer: [int *const p]
   forbids rebinding [p] and permits [p[0] = 1], while [const int *p]
   is the other way round. *)
let writes_through (x : t) : bool =
  match x.inner with
  | Array a -> not (is_const a.base)
  | Pointer p | Reference p -> not (is_const p)
  | _ -> false

(* A const reference cannot be assigned through, so it denotes the
   referent's value and is read as the referent. A mutable one denotes
   storage the callee can write, which the substrate does not model, so
   it is left alone rather than silently read as a value. *)
let deref_const (x : t) : t option =
  match x.inner with Reference p when is_const x -> Some p | _ -> None

let strip_const (x : t) : t =
  if Qualifier.Set.mem Const x.qualifiers then
    { x with name = None; qualifiers = Qualifier.Set.remove Const x.qualifiers }
  else x

let strip_reference (x : t) : t =
  match x.inner with Reference p -> p | _ -> x

(* An array strips every dimension at once, a pointer strips one level:
   [int[8][8]] has element type [int] and [int **] has element type
   [int *]. Pointers are stripped because a read of [float *A] needs
   [float] as its element type. *)
let rec strip_array (x : t) : t =
  match x.inner with
  | Array a -> strip_array a.base
  | Pointer p -> if is_array p then strip_array p else p
  | _ -> x

let rec get_array_dims (x : t) : int option list =
  match x.inner with
  | Array a -> a.size :: get_array_dims a.base
  | Pointer p -> get_array_dims p
  | _ -> []

let get_array_length (x : t) : int list =
  let l = get_array_dims x in
  if List.exists Option.is_none l then [] else List.filter_map Fun.id l

let get_array_type (x : t) : string list =
  match x.inner with
  | Array _ | Pointer _ ->
      strip_array x |> to_string |> String.split_on_char ' '
      |> List.filter (fun s -> String.length s > 0)
  | _ -> []

let vector_lanes (x : t) : string list option =
  match (strip_const x).inner with
  | Vector v -> Some (Vector_size.lanes v.size)
  | _ -> None

let sizeof (x : t) : int option =
  match x.inner with
  | Pointer _ -> Some 8
  | Scalar s -> Some (Scalar.sizeof s)
  | _ -> None

(* [width] and [sizeof] differ only on [void]: [sizeof] answers how many
   bytes a value occupies, where [void] has no answer, while [width]
   answers how far [+ 1] moves a pointer, where GNU C says one. Keep them
   apart. *)
let width (x : t) : int option =
  match x.inner with
  | Scalar s -> Some (Scalar.sizeof s)
  | Vector v -> Some (Vector_size.to_int v.size * Scalar.sizeof v.scalar)
  | Void -> Some 1
  | Pointer _ -> Some 8
  | _ -> None

(* How far [+ 1] moves a pointer, which is one level rather than every
   dimension: [int **] steps by 8 where [strip_array] would say 4. A step
   exists only when one level leaves something that is not itself an
   array, so [int[4][4]] has none: one level there is a row, and a flat
   single-index view of it is not indexing in rows. *)
let pointee_size (x : t) : int option =
  let elem =
    match x.inner with
    | Pointer p -> Some p
    | Array a -> Some a.base
    | _ -> None
  in
  match elem with Some e when not (is_array e) -> width e | _ -> None

let to_scalar (x : t) : Scalar.t option =
  match x.inner with Scalar s -> Some s | _ -> None

let to_int_dom (x : t) : Int_dom.t option =
  x |> to_scalar |> Option.map Scalar.to_int_dom |> Option.join

let to_bounds (x : t) : Bounds.t option =
  x |> to_scalar |> Option.map Scalar.to_bounds |> Option.join

(* Being an integer is a question about the type, not about whether a bound
   can be written for it: a [long] has no writable upper end and is still an
   integer. A bool answers yes so that a local bool declaration is still
   modelled. *)
let is_int (x : t) : bool =
  match to_scalar x with
  | Some s -> Scalar.is_int s || Scalar.is_bool s
  | None -> false

let is_unsigned (x : t) : bool =
  match to_scalar x with Some s -> Scalar.is_unsigned s | None -> false

(* ---------------------------- the C parser ---------------------------- *)

let scalar_table : (string * Scalar.t) list =
  [
    ("char", Scalar.char);
    ("signed char", Scalar.char);
    ("int8_t", Scalar.char);
    ("unsigned char", Scalar.unsigned_char);
    ("uchar", Scalar.unsigned_char);
    ("uint8_t", Scalar.unsigned_char);
    ("short", Scalar.short);
    ("signed short", Scalar.short);
    ("int16_t", Scalar.short);
    ("unsigned short", Scalar.unsigned_short);
    ("ushort", Scalar.unsigned_short);
    ("uint16_t", Scalar.unsigned_short);
    ("int", Scalar.int);
    ("signed int", Scalar.int);
    ("int32_t", Scalar.int);
    ("unsigned int", Scalar.unsigned_int);
    ("uint", Scalar.unsigned_int);
    ("uint32_t", Scalar.unsigned_int);
    ("long", Scalar.long);
    ("signed long", Scalar.long);
    ("int64_t", Scalar.long);
    ("long long", Scalar.long);
    ("signed long long", Scalar.long);
    ("unsigned long", Scalar.unsigned_long);
    ("ulong", Scalar.unsigned_long);
    ("size_t", Scalar.unsigned_long);
    ("uint64_t", Scalar.unsigned_long);
    ("unsigned long long", Scalar.unsigned_long);
    ("bool", Scalar.bool);
    ("float", Scalar.float);
    ("double", Scalar.double);
  ]

let vector_table : (string * Scalar.t) list =
  [
    ("char", Scalar.char);
    ("uchar", Scalar.unsigned_char);
    ("short", Scalar.short);
    ("ushort", Scalar.unsigned_short);
    ("int", Scalar.int);
    ("uint", Scalar.unsigned_int);
    ("long", Scalar.long);
    ("ulong", Scalar.unsigned_long);
    ("longlong", Scalar.long);
    ("ulonglong", Scalar.unsigned_long);
    ("float", Scalar.float);
    ("double", Scalar.double);
  ]

let parse_vector (s : string) : (Vector_size.t * Scalar.t) option =
  let n = String.length s in
  if n < 2 then None
  else
    let base = String.sub s 0 (n - 1) in
    let ( let* ) = Option.bind in
    let* scalar = List.assoc_opt base vector_table in
    let* size = Vector_size.of_int (Char.code s.[n - 1] - Char.code '0') in
    Some (size, scalar)

let trailing_qualifier (s : string) : (string * Qualifier.t) option =
  List.find_map
    (fun (spelling, q) ->
      if String.ends_with ~suffix:spelling s then
        let n = String.length s - String.length spelling in
        if n = 0 then None
        else
          let c = s.[n - 1] in
          if c = ' ' || c = '*' then Some (String.trim (String.sub s 0 n), q)
          else None
      else None)
    Qualifier.spellings

let leading_qualifier (s : string) : (Qualifier.t * string) option =
  List.find_map
    (fun (spelling, q) ->
      let p = spelling ^ " " in
      if String.starts_with ~prefix:p s then
        Some
          (q, String.sub s (String.length p) (String.length s - String.length p))
      else None)
    Qualifier.spellings

let parse_dims (s : string) : int option list =
  String.split_on_char '[' s
  |> List.filter_map (fun part ->
      if String.length part = 0 then None
      else Some (int_of_string_opt (String.sub part 0 (String.length part - 1))))

let rec parse (s : string) : t =
  let s = String.trim s in
  { (parse_shape s) with name = Some s }

and parse_shape (s : string) : t =
  let add (q : Qualifier.t) (x : t) : t =
    { x with name = None; qualifiers = Qualifier.Set.add q x.qualifiers }
  in
  match trailing_qualifier s with
  | Some (rest, q) -> add q (parse rest)
  | None -> (
      match String.index_opt s '[' with
      | Some i ->
          let base = parse (String.sub s 0 i) in
          let rest = String.sub s i (String.length s - i) in
          let rest, pointer =
            if String.ends_with ~suffix:" *" rest then
              (String.sub rest 0 (String.length rest - 2), true)
            else (rest, false)
          in
          let array =
            List.fold_right
              (fun size base -> make (Array { base; size }))
              (parse_dims rest) base
          in
          if pointer then make (Pointer array) else array
      | None ->
          if String.ends_with ~suffix:"*" s then
            make (Pointer (parse (String.sub s 0 (String.length s - 1))))
          else if String.ends_with ~suffix:"&" s then
            (* [T &] and [T &&] both name the referent's storage. *)
            let s = String.sub s 0 (String.length s - 1) in
            let s =
              if String.ends_with ~suffix:"&" (String.trim s) then
                let s = String.trim s in
                String.sub s 0 (String.length s - 1)
              else s
            in
            make (Reference (parse s))
          else (
            match leading_qualifier s with
            | Some (q, rest) -> add q (parse rest)
            | None -> parse_leaf s))

and parse_leaf (s : string) : t =
  if s = "void" then make Void
  else
    match List.assoc_opt s scalar_table with
    | Some sc -> make (Scalar sc)
    | None -> (
        match parse_vector s with
        | Some (size, scalar) -> make (Vector { size; scalar })
        | None ->
            if
              String.starts_with ~prefix:"struct " s
              || String.starts_with ~prefix:"class " s
              || String.starts_with ~prefix:"union " s
            then make (Struct { members = [] })
            else if String.contains s '(' then make Function
            else make (Opaque s))

(* Clang supplies a desugared spelling only when the whole type is a
   typedef name, so the fallback is a whole-type reparse. The written
   spelling leads: [float2] desugars to [struct float2] and must stay a
   vector, while [time_t] is opaque as written and resolves to [long].
   Typedefs in element position, which clang never desugars, are covered
   by [scalar_table]. *)
let of_c_string ?(desugared : string option) (s : string) : t =
  let x = parse s in
  match (x.inner, desugared) with
  | Opaque _, Some d when String.trim d <> String.trim s ->
      { (parse d) with name = Some (String.trim s) }
  | _ -> x
