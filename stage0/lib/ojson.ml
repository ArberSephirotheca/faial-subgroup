type j_object = (string * Yojson.Basic.t) list

(* --- Helpers for the option type --- *)

(* Monadic let *)
let ( let* ) = Option.bind

(* Monadic pipe *)
let ( >>= ) = Option.bind

(* Converts a some to a none if the condition is not met *)
let ensure (predicate : 'a -> bool) (v : 'a) : 'a option =
  if predicate v then Some v else None

let unless (first : 'a option) (second : 'a option) : 'a option =
  if Option.is_some first then first else second

(* --- Helpers for lists --- *)

let get_nth (i : int) (l : 'a list) : 'a option = List.nth_opt l i

(* --- Helpers for Yojson --- *)

let cast_object (j : Yojson.Basic.t) : j_object option =
  match j with `Assoc l -> Some l | _ -> None

let cast_string (j : Yojson.Basic.t) : string option =
  match j with `String v -> Some v | _ -> None

let cast_bool (j : Yojson.Basic.t) : bool option =
  match j with `Bool v -> Some v | _ -> None

let cast_list (j : Yojson.Basic.t) : Yojson.Basic.t list option =
  match j with `List l -> Some l | _ -> None

let get_field (k : string) (kv : j_object) : Yojson.Basic.t option =
  List.assoc_opt k kv

(* Related to our c-to-json representation *)

let get_kind (o : j_object) : string option = get_field "kind" o >>= cast_string

let has_kind (ks : string list) (o : j_object) : bool =
  get_kind o
  (* Check if the kind is in 'ks' *)
  |> Option.map (fun (k : string) -> List.mem k ks)
  (* Convert bool option to bool *)
  |> Option.value ~default:false

let pp_js data =
  let result = Yojson.Basic.to_string data in
  let size = 300 in
  let len = String.length result in
  if len > size then String.sub result 0 size ^ " …" else result

let j_error (j : Yojson.Basic.t) (msg : string) () =
  raise (msg ^ "\n" ^ pp_js j ^ "\n" |> Common.mk_parse_error_s)

let expect (default : unit -> 'a) : 'a option -> 'a = function
  | Some v -> v
  | None -> default ()
