type t = Line of string | Block of t list | Nil

let to_string ?indent:(p = 4) (l : t list) : string =
  let b = Buffer.create 100 in
  let rec pp (accum : int) : t -> unit = function
    | Nil -> ()
    | Line s ->
        Common.repeat " " (p * accum) |> Buffer.add_string b;
        s |> Buffer.add_string b;
        "\n" |> Buffer.add_string b;
        ()
    | Block lines -> lines |> List.iter (pp (accum + 1))
  in
  List.iter (pp 0) l;
  Buffer.contents b

let print ?indent:(p = 4) (l : t list) : unit =
  print_string (to_string ~indent:p l)
