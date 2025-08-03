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

let print ?(ppf = Format.std_formatter) ?indent:(p = 4) : t list -> unit =
  let rec pp (accum : int) : t -> unit = function
    | Nil -> ()
    | Line s -> Format.fprintf ppf "%s%s\n" (Common.repeat " " (p * accum)) s
    | Block lines -> lines |> List.iter (pp (accum + 1))
  in
  List.iter (pp 0)
