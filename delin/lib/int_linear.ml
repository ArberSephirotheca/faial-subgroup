module Vector = struct
  type t = int array

  let of_list = Array.of_list
  let length = Array.length
  let get = Array.get

  let nth_opt (v : t) (i : int) : int option =
    if i >= 0 && i < Array.length v then Some v.(i) else None

  let select (v : t) (idxs : int list) : t =
    idxs |> List.map (fun i -> v.(i)) |> Array.of_list

  let equal (x : t) (y : t) : bool = x = y

  let to_string (v : t) : string =
    v
    |> Array.to_list
    |> List.map string_of_int
    |> String.concat "; "
    |> Printf.sprintf "[%s]"
end

module Matrix = struct
  type t = Vector.t array

  let of_rows (rows : Vector.t list) : t =
    let width = match rows with [] -> 0 | v :: _ -> Vector.length v in
    List.iter
      (fun v ->
        if Vector.length v <> width then
          invalid_arg "Matrix.of_rows: rows have differing lengths")
      rows;
    Array.of_list rows

  let rows (m : t) : int = Array.length m
  let cols (m : t) : int = if Array.length m = 0 then 0 else Vector.length m.(0)
  let get (m : t) (i : int) (j : int) : int = m.(i).(j)

  let rec det (a : t) : int =
    let n = Array.length a in
    if n = 0 then 1
    else
      let minor (j : int) : t =
        Array.init (n - 1) (fun r ->
            Array.init (n - 1) (fun c ->
                a.(r + 1).(if c < j then c else c + 1)))
      in
      a.(0)
      |> Array.to_list
      |> List.mapi (fun j x ->
             let sign = if j land 1 = 0 then 1 else -1 in
             sign * x * det (minor j))
      |> List.fold_left ( + ) 0

  let replace_col (j : int) (a : t) (b : Vector.t) : t =
    Array.mapi
      (fun i row -> Array.mapi (fun k x -> if k = j then b.(i) else x) row)
      a

  let select_rows (m : t) (idxs : int list) : t =
    idxs |> List.map (fun i -> m.(i)) |> Array.of_list

  let cramer_solve (a : t) (b : Vector.t) : Vector.t option =
    let d = det a in
    if d = 0 then None
    else
      let xs = List.init (Array.length a) (fun j -> det (replace_col j a b)) in
      if List.for_all (fun x -> x mod d = 0) xs then
        Some (Vector.of_list (List.map (fun x -> x / d) xs))
      else None

  let dot (row : Vector.t) (x : Vector.t) : int =
    Array.fold_left ( + ) 0 (Array.map2 ( * ) row x)

  let solves (m : t) ~(x : Vector.t) ~(b : Vector.t) : bool =
    Vector.equal (Array.map (fun row -> dot row x) m) b

  let to_string (m : t) : string =
    m
    |> Array.to_list
    |> List.map (fun row -> "  " ^ Vector.to_string row)
    |> String.concat "\n"
end

let rec combinations (k : int) (xs : 'a list) : 'a list list =
  if k = 0 then [ [] ]
  else
    match xs with
    | [] -> []
    | x :: rest ->
        List.map (fun c -> x :: c) (combinations (k - 1) rest)
        @ combinations k rest

let int_solve (m : Matrix.t) (b : Vector.t) : Vector.t option =
  let ( let* ) = Option.bind in
  if Vector.length b <> Matrix.rows m then
    invalid_arg "Int_linear.int_solve: vector length must equal the row count";
  let c = Matrix.cols m in
  List.init (Matrix.rows m) Fun.id
  |> combinations c
  |> List.to_seq
  |> Seq.filter_map (fun idxs ->
         let a = Matrix.select_rows m idxs in
         let b' = Vector.select b idxs in
         let* x = Matrix.cramer_solve a b' in
         if Matrix.solves m ~x ~b then Some x else None)
  |> Seq.uncons
  |> Option.map fst

let tri_solve (l : Matrix.t) (b : Vector.t) : Vector.t option =
  let n = Matrix.rows l in
  if Vector.length b <> n then
    invalid_arg "Int_linear.tri_solve: vector length must equal the row count";
  let rec go (k : int) (solved : int list) : int list option =
    if k = n then Some solved
    else
      let sub =
        solved
        |> List.mapi (fun j xj -> Matrix.get l k j * xj)
        |> List.fold_left ( + ) 0
      in
      let s = Vector.get b k - sub in
      let d = Matrix.get l k k in
      if d = 0 || s mod d <> 0 then None
      else go (k + 1) (solved @ [ s / d ])
  in
  go 0 [] |> Option.map (fun xs -> Vector.of_list xs)
