type t = SharedMemory | GlobalMemory | ConstantMemory

let to_string : t -> string = function
  | SharedMemory -> "shared"
  | GlobalMemory -> "global"
  | ConstantMemory -> "constant"

let is_global : t -> bool = function
  | GlobalMemory -> true
  | SharedMemory | ConstantMemory -> false

let is_shared : t -> bool = function
  | SharedMemory -> true
  | GlobalMemory | ConstantMemory -> false

let is_constant : t -> bool = function
  | ConstantMemory -> true
  | SharedMemory | GlobalMemory -> false
