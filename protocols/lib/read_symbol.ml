let prefix : string = "$read_"

let name (array : Variable.t) : string = prefix ^ Variable.name array

let array_of (symbol : string) : Variable.t option =
  if String.starts_with ~prefix symbol then
    let n = String.length prefix in
    Some
      (Variable.from_name
         (String.sub symbol n (String.length symbol - n)))
  else None
