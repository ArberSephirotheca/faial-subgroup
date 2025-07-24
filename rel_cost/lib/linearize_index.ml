open Stage0
open Protocols

(*
  Converts from multiple-dimension accesses to a single dimension.
*)

type array_size = { byte_count : int; dim : int list }

module Make (L : Logger.Logger) = struct
  (* Given an n-dimensional array access apply type modifiers *)
  let shared_multiplier ~bytes_per_word ~byte_count (l : Exp.nexp list) :
      Exp.nexp list =
    if byte_count / bytes_per_word = 1 then l
    else
      let open Exp in
      let n_s = Exp.n_to_string in
      let bs = string_of_int byte_count ^ "/" ^ string_of_int bytes_per_word in
      let arr l = List.map n_s l |> String.concat ", " in
      let l' =
        List.map
          (fun n -> n_mult (Num byte_count) (n_div n (Num bytes_per_word)))
          l
      in
      L.info ("Applied byte-modifier : " ^ bs ^ " " ^ arr l ^ " -> " ^ arr l');
      l'

  (* Given an n-dimensional array access apply type modifiers *)
  let global_multiplier ~byte_count (l : Exp.nexp list) : Exp.nexp list =
    let open Exp in
    let n_s = Exp.n_to_string in
    let bs = string_of_int byte_count in
    let arr l = List.map n_s l |> String.concat ", " in
    let l' = List.map (fun n -> n_mult (Num byte_count) n) l in
    L.info ("Applied byte-modifier : " ^ bs ^ " " ^ arr l ^ " -> " ^ arr l');
    l'

  (* Convert an n-dimensional array access into a 1-d array access *)
  let flatten_multi_dim (dim : int list) (l : Exp.nexp list) : Exp.nexp =
    match l with
    | [ e ] -> e
    | _ ->
        let open Exp in
        (* Accumulate the values so that when we have
        [2, 2, 2] -> [1, 2, 4]
        *)
        let dim =
          dim |> List.rev
          |> List.fold_left (fun (mult, l) n -> (n * mult, mult :: l)) (1, [])
          |> snd
        in
        List.fold_right
          (fun (n, offset) accum -> n_plus (n_mult n (Num offset)) accum)
          (Common.zip l dim) (Num 0)

  (* Given a map of memory descriptors, return a map of array sizes *)
  let get_sizes ~bytes_per_word (mem : Memory.t Variable.Map.t) :
      array_size Variable.Map.t =
    mem
    |> Variable.Map.filter_map (fun _ v ->
           let open Memory in
           let ty = String.concat " " v.data_type |> C_type.make in
           match C_type.sizeof ty with
           | Some n -> Some { byte_count = n; dim = v.size }
           | None -> Some { byte_count = bytes_per_word; dim = v.size })

  (* Flatten n-dimensional array and apply word size *)
  let linearize (cfg : Config.t) (mem : Memory.t Variable.Map.t) :
      Variable.t -> Exp.nexp list -> Exp.nexp option =
    let sizes = get_sizes mem ~bytes_per_word:cfg.bytes_per_word in
    fun x l ->
      Variable.Map.find_opt x sizes
      |> Option.map (fun a ->
             l
             |> (if Variable.Map.find x mem |> Memory.is_shared then
                   shared_multiplier ~bytes_per_word:cfg.bytes_per_word
                     ~byte_count:a.byte_count
                 else global_multiplier ~byte_count:a.byte_count)
             |> flatten_multi_dim a.dim |> Constfold.n_opt)
end

module Silent = Make (Logger.Silent)
module Default = Make (Logger.Colors)
