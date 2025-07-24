type 'a stream = { stream_run : ('a -> unit) -> unit }

let iter (f : 'a -> unit) (s : 'a stream) : unit = s.stream_run f

let fold (f : 'a -> 'b -> 'b) (stream : 'a stream) (init : 'b) : 'b =
  let result = ref init in
  iter (fun x -> result := f x !result) stream;
  !result

let to_list (s : 'a stream) : 'a list =
  fold (fun x y -> x :: y) s [] |> List.rev

let from_list (l : 'a list) : 'a stream =
  { stream_run = (fun f -> List.iter f l) }

let map (f : 'a -> 'b) (s : 'a stream) : 'b stream =
  { stream_run = (fun handler -> s.stream_run (fun elem -> handler (f elem))) }

let mapi (f : int -> 'a -> 'b) : 'a stream -> 'b stream =
  let counter = ref 0 in
  map (fun x ->
      let old = !counter in
      counter := old + 1;
      f old x)

let take (n : int) (s : 'a stream) : 'a list =
  let counter = ref n in
  let data = ref [] in
  try
    iter
      (fun x ->
        let c = !counter in
        if c <= 0 then raise Exit else counter := c - 1;
        data := x :: !data;
        ())
      s;
    failwith "unexpected"
  with Exit -> List.rev !data

let empty : 'a stream = { stream_run = (fun _ -> ()) }
let one (x : 'a) : 'a stream = { stream_run = (fun handler -> handler x) }

let always (k : 'a) : 'a stream =
  {
    stream_run =
      (fun handler ->
        let rec f () =
          handler k;
          f ()
        in
        f ());
  }

let filter (p : 'a -> bool) (s : 'a stream) : 'a stream =
  {
    stream_run =
      (fun handler ->
        s.stream_run (fun elem -> if p elem then handler elem else ()));
  }

let filter_map (f : 'a -> 'b option) (stream : 'a stream) : 'b stream =
  map f stream
  |> filter (function Some _ -> true | None -> false)
  |> map (function Some x -> x | None -> failwith "unexpected")

let sequence (stream1 : 'a stream) (stream2 : 'a stream) : 'a stream =
  {
    stream_run =
      (fun f ->
        stream1.stream_run f;
        stream2.stream_run f);
  }

let lazy_sequence (stream1 : 'a stream) (stream2 : unit -> 'a stream) :
    'a stream =
  {
    stream_run =
      (fun f ->
        stream1.stream_run f;
        (stream2 ()).stream_run f);
  }

let concat (streams : 'a stream stream) : 'a stream =
  {
    stream_run =
      (fun handler ->
        streams.stream_run (fun sub_stream ->
            sub_stream.stream_run (fun elem -> handler elem)));
  }

let flat_map (f : 'a -> 'b stream) (stream : 'a stream) : 'b stream =
  map f stream |> concat

(* Consumes the first stream entirely *)
let product (s1 : 'a stream) (s2 : 'b stream) : ('a * 'b) stream =
  let xs : 'a list = to_list s1 in
  s2 |> map (fun y -> List.map (fun x -> (x, y)) xs |> from_list) |> concat
