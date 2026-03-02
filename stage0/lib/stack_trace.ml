type 'a t = RootCause of 'a | Because of 'a * 'a t

let rec fold_because (accum : 'a -> 'b -> 'a) (init : 'a) (s : 'b t) : 'a =
  match s with
  | RootCause x -> accum init x
  | Because (x, s) -> fold_because accum (accum init x) s

let rec fold_root_cause (accum : 'a -> 'b -> 'a) (s : 'a t) (init : 'b) : 'b =
  match s with
  | RootCause x -> accum x init
  | Because (x, s) -> accum x (fold_root_cause accum s init)

let iter (f : 'a -> unit) : 'a t -> unit = fold_because (fun _ x -> f x) ()

let rev_iter (f : 'a -> unit) (s : 'a t) : unit =
  fold_root_cause (fun x _ -> f x) s ()

let count (s : 'a t) : int = fold_because (fun count _ -> 1 + count) 0 s

let iteri (f : int -> 'a -> unit) (s : 'a t) : unit =
  let _ =
    fold_because
      (fun c x ->
        f c x;
        c - 1)
      (count s - 1)
      s
  in
  ()

let rev_iteri (f : int -> 'a -> unit) (s : 'a t) : unit =
  let _ =
    fold_root_cause
      (fun x c ->
        f c x;
        1 + c)
      s 0
  in
  ()

let to_list (s : 'a t) : 'a list = fold_because (fun l x -> x :: l) [] s

let rec map (f : 'a -> 'b) : 'a t -> 'b t = function
  | RootCause x -> RootCause (f x)
  | Because (x, e) -> Because (f x, map f e)
