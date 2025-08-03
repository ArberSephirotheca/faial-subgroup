(*

 Utility functions on top of fpath.

 *)

let join (x : Fpath.t list) : Fpath.t =
  match x with
  | [] -> raise (invalid_arg "empty list")
  | [ x ] -> x
  | x :: l -> List.fold_left Fpath.append x l

let from_list (x : string list) : Fpath.t = List.map Fpath.v x |> join

(* cross platform load path, expects path separator to be / *)
let from_string (x : string) : Fpath.t =
  x |> String.split_on_char '/' |> from_list

let read_dir (dir : Fpath.t) : Fpath.t list =
  Sys.readdir (Fpath.to_string dir)
  |> Array.map (fun x -> Fpath.append dir (Fpath.v x))
  |> Array.to_list

let exists (f : Fpath.t) : bool = Sys.file_exists (Fpath.to_string f)
