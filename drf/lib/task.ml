type t = Task1 | Task2

let to_string : t -> string = function Task1 -> "T1" | Task2 -> "T2"
let other : t -> t = function Task1 -> Task2 | Task2 -> Task1
