type t = BOr | BAnd

let eval : t -> bool -> bool -> bool = function BOr -> ( || ) | BAnd -> ( && )
let to_string : t -> string = function BOr -> "||" | BAnd -> "&&"
