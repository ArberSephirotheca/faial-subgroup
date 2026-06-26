let rec det (a : int list list) : int =
  match a with
  | [] -> 1
  | row :: rest ->
    let remove_col j r = List.filteri (fun i _ -> i <> j) r in
    row
    |> List.mapi (fun j x ->
         let sign = if j land 1 = 0 then 1 else -1 in
         sign * x * det (List.map (remove_col j) rest))
    |> List.fold_left ( + ) 0

let replace_col (j : int) (a : int list list) (b : int list) : int list list =
  List.map2
    (fun row bi -> List.mapi (fun k x -> if k = j then bi else x) row)
    a b

let dot (r : int list) (x : int list) : int =
  List.fold_left ( + ) 0 (List.map2 ( * ) r x)

let matvec (w : int list list) (x : int list) : int list =
  List.map (fun row -> dot row x) w

let solve_square (a : int list list) (b : int list) : int list option =
  let d = det a in
  if d = 0 then None
  else
    let xs = List.init (List.length a) (fun j -> det (replace_col j a b)) in
    if List.for_all (fun x -> x mod d = 0) xs
    then Some (List.map (fun x -> x / d) xs)
    else None

let rec combinations (k : int) (xs : 'a list) : 'a list list =
  if k = 0 then [ [] ]
  else
    match xs with
    | [] -> []
    | x :: rest ->
      List.map (fun c -> x :: c) (combinations (k - 1) rest)
      @ combinations k rest

let int_solve (w : int list list) (b : int list) : int list option =
  let ( let* ) = Option.bind in
  let c = match w with [] -> 0 | row :: _ -> List.length row in
  let warr = Array.of_list w in
  let barr = Array.of_list b in
  List.init (List.length w) Fun.id
  |> combinations c
  |> List.to_seq
  |> Seq.filter_map (fun idxs ->
       let a = List.map (fun i -> warr.(i)) idxs in
       let b' = List.map (fun i -> barr.(i)) idxs in
       let* x = solve_square a b' in
       if matvec w x = b then Some x else None)
  |> Seq.uncons
  |> Option.map fst
