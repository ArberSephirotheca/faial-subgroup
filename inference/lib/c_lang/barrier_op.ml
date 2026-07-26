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

(* Strict type guard on a resolved C type. A barrier is not modelled, so
   it parses to [Opaque], whose payload carries the resolved spelling
   even when the declaration was written through a typedef. *)
let is_barrier_c_type (ty : Ty.t) : bool =
  match Ty.to_opaque ty with
  | Some s -> Common.contains ~substring:"cuda::barrier" s
  | None -> false

(* Strict type guard: the desugared base type must be cuda::barrier<_>. *)
let is_barrier_base_type (ty : Ty.t) : bool =
  is_barrier_c_type ty
