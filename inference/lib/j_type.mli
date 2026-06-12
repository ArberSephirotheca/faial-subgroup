open Stage0
open Protocols

type t

(* Constructors *)
val from_json : Yojson.Basic.t -> t
val from_string : string -> t
val from_c_type : C_type.t -> t

(* Common types *)
val int : t
val char : t
val bool : t
val float : t
val void : t
val unknown : t

(* Map into c_types *)
val matches : (C_type.t -> bool) -> t -> bool

(* Like [matches], but checks the desugared (typedefs resolved) type.
   Falls back to qualType when no desugared form is present. *)
val desugared_matches : (C_type.t -> bool) -> t -> bool

(* Type conversion *)
val to_c_type_res : t -> C_type.t Rjson.j_result
val to_c_type : ?default:C_type.t -> t -> C_type.t
val to_desugared_c_type_res : t -> C_type.t Rjson.j_result
val to_desugared_c_type : ?default:C_type.t -> t -> C_type.t
val to_string : t -> string
