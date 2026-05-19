open Stage0
open Protocols

type t = Arrive | Wait | ArriveAndWait | ArriveAndDrop

let to_string : t -> string = function
  | Arrive -> "arrive"
  | Wait -> "wait"
  | ArriveAndWait -> "arrive_and_wait"
  | ArriveAndDrop -> "arrive_and_drop"

let of_method_name : string -> t option = function
  | "arrive" -> Some Arrive
  | "wait" -> Some Wait
  | "arrive_and_wait" -> Some ArriveAndWait
  | "arrive_and_drop" -> Some ArriveAndDrop
  | _ -> None

(* Strict type guard on a resolved C type. *)
let is_barrier_c_type (ty : C_type.t) : bool =
  Common.contains ~substring:"cuda::barrier" (C_type.to_string ty)

(* Strict type guard: the desugared base type must be cuda::barrier<_>. *)
let is_barrier_base_type (ty : J_type.t) : bool =
  J_type.desugared_matches is_barrier_c_type ty
