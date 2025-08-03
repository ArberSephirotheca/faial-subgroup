(** State monad *)

type ('s, 'a) t = 's -> 's * 'a

(** Return function: takes a value and returns a state function that produces
    that value without changing the state *)
let return (a : 'a) : ('s, 'a) t = fun (s : 's) -> (s, a)

(** Bind function: chains stateful computations *)
let bind (m : ('s, 'a) t) (f : 'a -> ('s, 'b) t) : ('s, 'b) t =
 fun s ->
  let s', a = m s in
  f a s'

(** Most general constructor, which sets the current state and returns a value
*)
let update_return (f : 's -> 's * 'a) : ('s, 'a) t = f

(** Get and return *)
let get_return (f : 's -> 'a) : ('s, 'a) t = fun s -> (s, f s)

(** Update the current state *)
let update (f : 's -> 's) : ('s, unit) t = fun s -> (f s, ())

(** Update the current state *)
let put (s : 's) : ('s, unit) t = update (fun _ -> s)

(** Read the current state *)
let get : ('s, 's) t = fun s -> (s, s)

(** Given a state and a monad, return the final state and the output result *)
let run (st : 'st) (m : ('s, 'a) t) : 's * 'a = m st

(** Monad syntax for let*: useful for making state monad operations readable *)
module Syntax = struct
  let ( let* ) = bind
  let ( >>= ) = bind
  let return = return
end

let list_map (f : 'a -> ('s, 'b) t) (l : 'a list) : ('s, 'b list) t =
  let open Syntax in
  let rec handle_list (l : 'a list) : ('s, 'b list) t =
    match l with
    | [] -> return []
    | x :: l ->
        let* x = f x in
        let* l = handle_list l in
        return (x :: l)
  in
  handle_list l

let option_map (f : 'a -> ('s, 'b) t) (o : 'a option) : ('s, 'b option) t =
  let open Syntax in
  match o with
  | Some v ->
      let* v = f v in
      return (Some v)
  | None -> return None
