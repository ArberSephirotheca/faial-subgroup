type t = SharedMemory | GlobalMemory

let to_string : t -> string = function
  | SharedMemory -> "shared"
  | GlobalMemory -> "global"

let is_global (x : t) : bool = x = GlobalMemory
let is_shared (x : t) : bool = x = SharedMemory
