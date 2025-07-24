module StackTrace = Stack_trace

type j_object = (string * Yojson.Basic.t) list
type j_list = Yojson.Basic.t list

(* --- Helpers for the option type --- *)

(* Monadic let *)
let ( let* ) = Result.bind

(* Monadic pipe *)
let ( >>= ) = Result.bind

let unless (first : ('a, 'e) Result.t) (second : ('a, 'e) Result.t) :
    ('a, 'e) Result.t =
  if Result.is_ok first then first else second

(* --- Helpers for Yojson --- *)

let type_name : Yojson.Basic.t -> string = function
  | `Assoc _ -> "object"
  | `Bool _ -> "bool"
  | `Float _ | `Int _ -> "number"
  | `List _ -> "list"
  | `Null -> "null"
  | `String _ -> "string"

type j_error = (string * Yojson.Basic.t) StackTrace.t

let pp_js data =
  let result = Yojson.Basic.to_string data in
  let size = 300 in
  let len = String.length result in
  if len > size then String.sub result 0 size ^ " …" else result

let iter_error (on_msg : string -> unit) : j_error -> unit =
  StackTrace.iteri (fun c (s, j) ->
      match c with
      | 0 ->
          on_msg s;
          on_msg (Yojson.Basic.pretty_to_string j)
      | _ -> on_msg (s ^ ": " ^ pp_js j))

let print_error : j_error -> unit = iter_error prerr_endline

let error_to_buffer (e : j_error) : Buffer.t =
  let b = Buffer.create 512 in
  iter_error (Buffer.add_string b) e;
  b

let error_to_string (e : j_error) : string =
  error_to_buffer e |> Buffer.contents

type 'a j_result = ('a, j_error) Result.t

let root_cause (msg : string) (j : Yojson.Basic.t) : 'a j_result =
  Result.Error (StackTrace.RootCause (msg, j))

let because (msg : string) (j : Yojson.Basic.t) (e : j_error) : 'a j_result =
  Result.Error (StackTrace.Because ((msg, j), e))

let add_reason : 'a. string -> Yojson.Basic.t -> 'a j_result -> 'a j_result =
 fun msg j e ->
  e |> Result.map_error (fun e -> StackTrace.Because ((msg, j), e))

let type_mismatch ty j =
  root_cause ("type mismatch: expecting " ^ ty ^ ", but got " ^ type_name j) j

let map_all (f : 'a -> ('b, 'e) Result.t) (err : int -> 'a -> 'e -> 'e)
    (l : 'a list) : ('b list, 'e) Result.t =
  let rec map (idx : int) (l : 'a list) =
    match l with
    | x :: l -> (
        match f x with
        | Ok x ->
            let* l = map (idx + 1) l in
            Ok (x :: l)
        | Error e -> Error (err idx x e))
    | [] -> Ok []
  in
  map 0 l

let cast_object (j : Yojson.Basic.t) : j_object j_result =
  match j with `Assoc l -> Ok l | _ -> type_mismatch "object" j

let cast_option (f : Yojson.Basic.t -> 'a j_result) :
    Yojson.Basic.t -> 'a option j_result = function
  | `Null -> Ok None
  | j -> f j |> Result.map Option.some

let cast_string (j : Yojson.Basic.t) : string j_result =
  match j with `String v -> Ok v | _ -> type_mismatch "string" j

let cast_bool (j : Yojson.Basic.t) : bool j_result =
  match j with `Bool v -> Ok v | _ -> type_mismatch "bool" j

let cast_int (j : Yojson.Basic.t) : int j_result =
  match j with `Int v -> Ok v | _ -> type_mismatch "int" j

let cast_float (j : Yojson.Basic.t) : float j_result =
  match j with `Float v -> Ok v | _ -> type_mismatch "float" j

let cast_list (j : Yojson.Basic.t) : Yojson.Basic.t list j_result =
  match j with `List l -> Ok l | _ -> type_mismatch "list" j

let cast_cons (j : Yojson.Basic.t) :
    (Yojson.Basic.t * Yojson.Basic.t list) j_result =
  let* l = cast_list j in
  match l with
  | h :: t -> Ok (h, t)
  | [] ->
      root_cause "Expecting a nonempty list, but got an empty list instead" j

let map (f : 'a -> ('b, 'e) Result.t) : 'a list -> 'b list j_result =
  map_all f (fun idx s e ->
      StackTrace.Because (("Error in index #" ^ string_of_int (idx + 1), s), e))

(* Cast the given json as a list, and then cast every element of the list *)
let cast_map (f : 'a -> ('b, 'e) Result.t) (j : Yojson.Basic.t) :
    'b list j_result =
  cast_list j >>= map f

let ensure_length_eq (len : int) (l : Yojson.Basic.t list) :
    Yojson.Basic.t list j_result =
  if List.length l = len then Ok l
  else
    let e = string_of_int len in
    let g = string_of_int (List.length l) in
    root_cause
      ("Expecting a list of length " ^ e ^ ", but got length " ^ g)
      (`List l)

let cast_list_1 (handler : Yojson.Basic.t -> 'a j_result) (j : Yojson.Basic.t) :
    'a j_result =
  let* l = cast_list j in
  match l with
  | [ x ] -> (
      match handler x with
      | Ok x -> Ok x
      | Error e -> because "Error in index #1" x e)
  | _ ->
      let g = string_of_int (List.length l) in
      root_cause
        ("Expecting a list of length 1, but got a list of length " ^ g)
        (`List l)

let cast_first (handler : Yojson.Basic.t -> 'a j_result) (j : Yojson.Basic.t) :
    'a j_result =
  let* l = cast_list j in
  match l with
  | x :: _ -> (
      match handler x with
      | Ok x -> Ok x
      | Error e -> because "Error in index #1" x e)
  | _ ->
      let g = string_of_int (List.length l) in
      root_cause
        ("Expecting a list of length 1, but got a list of length " ^ g)
        (`List l)

let cast_list_2 (handle_fst : Yojson.Basic.t -> 'a j_result)
    (handle_snd : Yojson.Basic.t -> 'b j_result) (j : Yojson.Basic.t) :
    ('a * 'b) j_result =
  let* l = cast_list j in
  match l with
  | [ x; y ] -> (
      match handle_fst x with
      | Ok x -> (
          match handle_snd y with
          | Ok y -> Ok (x, y)
          | Error e -> because "Error in index #2" y e)
      | Error e -> because "Error in index #1" x e)
  | _ ->
      let g = string_of_int (List.length l) in
      root_cause
        ("Expecting a list of length 2, but got a list of length " ^ g)
        (`List l)

let cast_list_3 (handle_fst : Yojson.Basic.t -> 'a j_result)
    (handle_snd : Yojson.Basic.t -> 'b j_result)
    (handle_third : Yojson.Basic.t -> 'c j_result) (j : Yojson.Basic.t) :
    ('a * 'b * 'c) j_result =
  let* l = cast_list j in
  match l with
  | [ x; y; z ] -> (
      match handle_fst x with
      | Ok x -> (
          match handle_snd y with
          | Ok y -> (
              match handle_third z with
              | Ok z -> Ok (x, y, z)
              | Error e -> because "Error in index #3" z e)
          | Error e -> because "Error in index #2" y e)
      | Error e -> because "Error in index #1" x e)
  | _ ->
      let g = string_of_int (List.length l) in
      root_cause
        ("Expecting a list of length 3, but got a list of length " ^ g)
        (`List l)

let get_field (k : string) (kv : j_object) : Yojson.Basic.t j_result =
  match List.assoc_opt k kv with
  | Some v -> Ok v
  | None ->
      let avail_keys =
        kv |> List.map (fun (k, _) -> "'" ^ k ^ "'") |> String.concat ", "
        |> fun k -> "[" ^ k ^ "]"
      in
      root_cause
        ("Field missing '" ^ k ^ "', available keys: " ^ avail_keys)
        (`Assoc kv)

(* Related to our c-to-json representation *)

let because_field (k : string) (o : j_object) (e : j_error) : j_error =
  StackTrace.Because (("field: " ^ k, `Assoc o), e)

let with_field (k : string) (f : Yojson.Basic.t -> 'a j_result) (o : j_object) :
    'a j_result =
  get_field k o
  >>= Common.wrap f (* If we can get the field, then pass the field to 'f' *)
        (fun e -> Error (because_field k o e))

let with_opt_field (k : string) (f : Yojson.Basic.t -> 'a j_result)
    (o : j_object) : 'a option j_result =
  match List.assoc_opt k o with
  | Some field -> f field |> Result.map (fun x -> Some x)
  | None -> Ok None

let with_field_or (k : string) (f : Yojson.Basic.t -> 'a j_result)
    (default : 'a) (o : j_object) : 'a j_result =
  match List.assoc_opt k o with
  | Some field -> f field |> Result.map_error (because_field k o)
  | None -> Ok default

let get_index (i : int) (l : j_list) : Yojson.Basic.t j_result =
  match List.nth_opt l i with
  | Some v -> Ok v
  | None ->
      let i = string_of_int (i + 1) in
      let len = string_of_int (List.length l) in
      let msg =
        "Index out of bounds, retriving position #" ^ i
        ^ ", but list has length " ^ len
      in
      root_cause msg (`List l)

let because_get_index (i : int) (l : Yojson.Basic.t list) :
    'a j_result -> 'a j_result =
  Result.map_error (fun e ->
      StackTrace.Because (("Position #" ^ string_of_int (i + 1), `List l), e))

let wrap (ok_handler : 'a -> 'b j_result)
    (error_handler : j_error -> string * Yojson.Basic.t) (v : 'a) =
  Common.wrap ok_handler
    (fun (e : j_error) ->
      let msg, j = error_handler e in
      because msg j e)
    v

let with_index (index : int) (f : Yojson.Basic.t -> 'a j_result) (l : j_list) :
    'a j_result =
  get_index index l
  >>= wrap f (fun _ -> ("Position #" ^ string_of_int (index + 1), `List l))

let get_kind (o : j_object) : string j_result = with_field "kind" cast_string o

let filter_kind (f : string -> bool) (o : j_object) : bool =
  get_kind o
  (* Check if the kind is in 'ks' *)
  |> Result.map f
  (* Convert bool option to bool *)
  |> Result.value ~default:false

let has_kind (ks : string list) (o : j_object) : bool =
  filter_kind (fun (k : string) -> List.mem k ks) o
