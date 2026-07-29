module StringOT = struct
  type t = string

  let compare = Stdlib.compare
end

module MapUtil (M : Map.S) = struct
  let union_left (m1 : 'a M.t) (m2 : 'a M.t) : 'a M.t =
    M.union (fun _ o _ -> Some o) m1 m2

  let union_right (m1 : 'a M.t) (m2 : 'a M.t) : 'a M.t = union_left m2 m1
end

module MapSetUtil (S : Set.S) (M : Map.S with type key = S.elt) = struct
  let set_to_map (s : S.t) (f : S.elt -> 'a option) : 'a M.t =
    S.fold
      (fun k m -> match f k with Some v -> M.add k v m | None -> m)
      s M.empty

  let map_to_set (m : 'a M.t) : S.t = M.bindings m |> List.map fst |> S.of_list
end

module StringSet = Set.Make (StringOT)
module StringMap = Map.Make (StringOT)
module StringMapUtil = MapUtil (StringMap)
module StringMapSetUtil = MapSetUtil (StringSet) (StringMap)

module IntMap = Map.Make (struct
  type t = int

  let compare = compare
end)

module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

let append_tr (l1 : 'a list) (l2 : 'a list) : 'a list =
  let rec app (ret : 'a list -> 'a list) (l : 'a list) : 'a list =
    match l with
    | [] -> ret l2
    | x :: l -> app (fun new_l -> ret (x :: new_l)) l
  in
  app (fun x -> x) l1

(* Concatenates a reversed version of lhs to rhs *)
let rec append_rev1 (l1 : 'a list) (l2 : 'a list) : 'a list =
  match l1 with [] -> l2 | x :: l1 -> append_rev1 l1 (x :: l2)

(* Repeats string s n times *)
let rec repeat (s : string) (n : int) : string =
  if n <= 0 then "" else s ^ repeat s (n - 1)

let rsplit (c : char) (s : string) : (string * string) option =
  String.rindex_opt s c
  |> Option.map (fun idx ->
      (String.sub s 0 idx, String.sub s (idx + 1) (String.length s - idx - 1)))

let split (c : char) (s : string) : (string * string) option =
  String.index_opt s c
  |> Option.map (fun idx ->
      (String.sub s 0 idx, String.sub s (idx + 1) (String.length s - idx - 1)))

let last (l : 'a list) : ('a list * 'a) option =
  match l with
  | [] -> None
  | _ ->
      let rec iter (l : 'a list) =
        match l with
        | [ x ] -> ([], x)
        | [] -> failwith "Does not support emptylist"
        | x :: l ->
            let l, y = iter l in
            (x :: l, y)
      in
      Some (iter l)

let list_is_empty (l : 'a list) : bool = match l with [] -> true | _ -> false

let enumerate (l : 'a list) : (int * 'a) list =
  let rec iter idx l =
    match l with h :: t -> (idx, h) :: iter (idx + 1) t | _ -> []
  in
  iter 0 l

let flatten_opt : 'a option list -> 'a list =
 fun (l : 'a option list) -> List.concat_map Option.to_list l

let either_split (l : ('a, 'b) Either.t list) : 'a list * 'b list =
  List.partition_map (fun a -> a) l

let uniquify ~(name : 'a -> string) ~(rename : 'a -> string -> 'a)
    ~(taken : StringSet.t) (l : 'a list) : 'a list =
  let initial =
    List.fold_left (fun acc x -> StringSet.add (name x) acc) taken l
  in
  l
  |> List.fold_left
       (fun (used, acc) x ->
         let n = name x in
         if not (StringSet.mem n used) then (StringSet.add n used, x :: acc)
         else
           let rec fresh (i : int) : string =
             let candidate = Printf.sprintf "%s_%d" n i in
             if StringSet.mem candidate used || StringSet.mem candidate initial
             then fresh (i + 1)
             else candidate
           in
           let n = fresh 2 in
           (StringSet.add n used, rename x n :: acc))
       (taken, [])
  |> snd |> List.rev

let contains ~substring:(needle : string) (s : string) : bool =
  let n_len = String.length needle in
  let s_len = String.length s in
  if n_len = 0 then true
  else if n_len > s_len then false
  else
    (* Has at least one character *)
    let ch = needle.[0] in
    (* Find the every character that starts with `ch`
       extract a substring with as many chars than needle.
       Return true if neele matches substring. *)
    let rec contains (offset : int) : bool =
      match String.index_from_opt s offset ch with
      | Some idx ->
          if s_len - idx >= n_len then
            (* Check if we have enough characters to search *)
            let sub = String.sub s idx n_len in
            (* Substring matches needle, we are done *)
            if String.equal sub needle then true
            (* Try again *)
              else contains (idx + 1)
            (* Otherwise, not enough characters, impossible
           to find neelde. *)
          else false
      | None -> false
    in
    contains 0

let hashtbl_elements (t : ('a, 'b) Hashtbl.t) : ('a * 'b) list =
  Hashtbl.fold (fun k v accum -> (k, v) :: accum) t []

let hashtbl_update (ht : ('a, 'b) Hashtbl.t) (kvs : ('a * 'b) list) : unit =
  List.iter (fun (k, v) -> Hashtbl.replace ht k v) kvs

let hashtbl_from_list (kvs : ('a * 'b) list) : ('a, 'b) Hashtbl.t =
  let ht = Hashtbl.create (List.length kvs) in
  hashtbl_update ht kvs;
  ht

let rec zip (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
  match (l1, l2) with
  | [], _ | _, [] -> []
  | x :: l1, y :: l2 -> (x, y) :: zip l1 l2

let rec zip3 (l1 : 'a list) (l2 : 'b list) (l3 : 'c list) : ('a * 'b * 'c) list
    =
  match (l1, l2, l3) with
  | [], _, _ | _, [], _ | _, _, [] -> []
  | x :: l1, y :: l2, z :: l3 -> (x, y, z) :: zip3 l1 l2 l3

let range ?(from = 0) (until : int) : int list =
  let rec iter (curr : int) (acc : int list) : int list =
    if curr < from then acc else iter (curr - 1) (curr :: acc)
  in
  iter until []

(* https://stackoverflow.com/a/46759007/2327050 *)
let modulo (x : int) (y : int) : int =
  let result = x mod y in
  if result >= 0 then result else result + y

exception ParseError of Buffer.t

let string_to_buffer (s : string) =
  let b = Buffer.create (String.length s) in
  Buffer.add_string b s;
  b

let replace ~substring ~by : string -> string =
  Str.global_replace (Str.regexp_string substring) by

let buffer_prepend (b : Buffer.t) (s : string) : Buffer.t =
  let result = Buffer.create (Buffer.length b + String.length s) in
  Buffer.add_string result s;
  Buffer.add_buffer result b;
  result

let mk_parse_error_s s = ParseError (string_to_buffer s)

let mk_parse_error_l ss =
  let b = Buffer.create (16 * List.length ss) in
  List.iter
    (fun x ->
      Buffer.add_string b x;
      Buffer.add_char b '\n')
    ss;
  b

let is_even n = n mod 2 = 0

let expect (s : string) (o : 'a option) : 'a =
  match o with Some v -> v | None -> failwith s

let pow ~base exponent : int =
  if exponent < 0 then invalid_arg "exponent can not be negative"
  else
    let rec aux accumulator base = function
      | 0 -> accumulator
      | 1 -> base * accumulator
      | e when is_even e -> aux accumulator (base * base) (e / 2)
      | e -> aux (base * accumulator) (base * base) ((e - 1) / 2)
    in
    aux 1 base exponent

let highest_power ~base (n : int) : int =
  let exponent : float =
    Float.log (Float.of_int n) /. Float.log (Float.of_int base)
  in
  pow ~base (Float.to_int exponent)

(*
  Takes an ok_handler and an error_handler.
  Applies ok_handler to v and if there's an error, route it to
  the error_handler.
  *)
let wrap (ok_handler : 'a -> ('b, 'e) Result.t)
    (error_handler : 'e -> ('b, 'e) Result.t) (v : 'a) : ('b, 'e) Result.t =
  match ok_handler v with Ok bv -> Ok bv | Error (e : 'e) -> error_handler e

let string_split_lines (x : string) : string list =
  let eol = Str.regexp "\r?\n" in
  Str.split eol x

(* Read a range of lines: offset has base 0 *)
let get_lines ~offset ~count (filename : string) : string list =
  (* Skip the first n-lines *)
  let rec skip_n ic count =
    if count <= 0 then ()
    else
      let _ = input_line ic in
      skip_n ic (count - 1)
  in
  (* Return the first n-lines *)
  let yield_n ic count = List.init count (fun _ -> input_line ic) in
  let ic = open_in filename in
  skip_n ic offset;
  let lines = yield_n ic count in
  close_in ic;
  lines

(* Read a single line *)
let get_line offset filename =
  match get_lines filename ~offset ~count:1 with
  | [ l ] -> l
  | _ -> failwith "Unexpected output"

let rec permutations_seq : 'a list -> 'a list Seq.t = function
  | [] -> Seq.return []
  | xs ->
    List.mapi (fun i x -> (i, x)) xs
    |> List.to_seq
    |> Seq.concat_map (fun (i, x) ->
         List.filteri (fun j _ -> j <> i) xs
         |> permutations_seq
         |> Seq.map (fun p -> x :: p))

let rec permutations : 'a list -> 'a list list = function
  | [] -> [ [] ]
  | xs ->
    List.mapi (fun i x -> (i, x)) xs
    |> List.concat_map (fun (i, x) ->
         List.filteri (fun j _ -> j <> i) xs
         |> permutations
         |> List.map (fun p -> x :: p))
