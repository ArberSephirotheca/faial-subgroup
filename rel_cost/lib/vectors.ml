open Stage0
module IntSet = Common.IntSet

let pointwise (f : 'a -> 'a -> 'b) (l1 : 'a list) (l2 : 'a list) : 'b list =
  assert (List.length l1 = List.length l2);
  List.map2 f l1 l2

let list_to_string (l : string list) : string = "[" ^ String.concat ", " l ^ "]"

type 'a index = { index : int; value : 'a }

module NMap : sig
  type t

  val make : int -> (int -> int) -> t
  val random : int -> unit -> t
  val constant : count:int -> value:int -> t
  val pointwise : (int -> int -> int) -> t -> t -> t
  val to_string : t -> string
  val map : (int -> int) -> t -> t
  val max : t -> int index
  val get : int -> t -> int
  val to_array : t -> int array
  val from_array : int array -> t
end = struct
  type t = int array

  let make (count : int) (f : int -> int) : t = Array.init count f

  let random (count : int) () =
    make count (fun _ -> Random.nativebits () |> Nativeint.to_int)

  let constant ~count ~value : t = Array.make count value
  let pointwise : (int -> int -> int) -> t -> t -> t = Array.map2

  let to_string (l : t) : string =
    l |> Array.map string_of_int |> Array.to_list |> list_to_string

  let map f v = Array.map f v
  let get idx a = Array.get a idx

  let max (x : t) : int index =
    x
    |> Array.mapi (fun idx v -> { index = idx; value = v })
    |> Array.fold_left
         (fun p1 p2 -> if p1.value >= p2.value then p1 else p2)
         { index = 0; value = Array.get x 0 }

  let to_array x = x
  let from_array x = x
end

module BMap : sig
  type t

  val make : int -> (int -> bool) -> t
  val constant : count:int -> value:bool -> t
  val pointwise : (bool -> bool -> bool) -> t -> t -> t
  val to_string : t -> string
  val some_true : t -> bool
  val all_false : t -> bool
  val map : (bool -> bool) -> t -> t
  val get : int -> t -> bool
  val to_array : t -> bool array
  val from_array : bool array -> t
  val count : bool -> t -> int
end = struct
  type t = bool array

  let make (count : int) (f : int -> bool) : t = Array.init count f
  let constant ~count ~value : t = Array.make count value
  let pointwise : (bool -> bool -> bool) -> t -> t -> t = Array.map2

  let to_string (l : t) : string =
    l
    |> Array.map (fun x -> if x then "true" else "false")
    |> Array.to_list |> list_to_string

  let some_true : t -> bool = Array.exists (fun v -> v)
  let all_false : t -> bool = Array.for_all (fun v -> not v)
  let get (x : int) (v : t) : bool = Array.get v x
  let map = Array.map
  let to_array x = x
  let from_array x = x

  let count (b : bool) (a : t) : int =
    Array.fold_left (fun sum b' -> if b = b' then sum + 1 else sum) 0 a
end

let n_map3 (f : bool -> int -> int -> int) (b : BMap.t) (n1 : NMap.t)
    (n2 : NMap.t) : NMap.t =
  let n1 : int array = NMap.to_array n1 in
  let n2 : int array = NMap.to_array n2 in
  let b : bool array = BMap.to_array b in
  Array.map2 (fun b (x1, x2) -> f b x1 x2) b (Array.combine n1 n2)
  |> NMap.from_array

let n_map2 (f : int -> int -> bool) (n1 : NMap.t) (n2 : NMap.t) : BMap.t =
  let n1 = NMap.to_array n1 in
  let n2 = NMap.to_array n2 in
  Array.map2 f n1 n2 |> BMap.from_array
