(* __global__ kernels are entry points and can be invoked from
  the host. Auxiliary kernels have a __device__ attribute and
  can only be invoked from the GPU code. *)
type t = Global | Device

let to_string : t -> string = function
  | Global -> "__global__"
  | Device -> "__device__"
