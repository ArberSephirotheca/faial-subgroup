(*
 Given a phase-split kernel, generate a location-split kernel.
 *)
open Stage0
open Protocols

module Kernel = struct
  type t = {
    (* The kernel name *)
    name : string;
    (* The shared locations that can be accessed in the kernel. *)
    array_name : string;
    (* The internal variables are used in the code of the kernel.  *)
    global_variables : Params.t;
    (* The internal variables are used in the code of the kernel.  *)
    local_variables : Params.t;
    (* Global ranges *)
    ranges : Range.t list;
    (* The code of a kernel performs the actual memory accesses. *)
    code : Unsync.t;
  }

  let free_names (k : t) : Variable.Set.t =
    Variable.Set.empty |> Unsync.free_names k.code
    |> List.fold_right Range.free_names k.ranges

  let to_s (k : t) : Indent.t list =
    let open Indent in
    let ranges = List.map Range.to_string k.ranges |> String.concat "; " in
    [
      Line ("array: " ^ k.array_name ^ ";");
      Line ("globals: " ^ Params.to_string k.global_variables ^ ";");
      Line ("locals: " ^ Params.to_string k.local_variables ^ ";");
      Line ("ranges: " ^ ranges ^ ";");
      Line "{";
      Block (Unsync.to_s k.code);
      Line "}";
    ]

  let from_phased (k : Phasesplit.Kernel.t) : t Streamutil.stream =
    Variable.Set.elements k.arrays
    |> Streamutil.from_list
    |> Streamutil.filter_map (fun x ->
        (* For every location *)
        match Unsync.filter_by_location x k.code with
        | Some p ->
            (* Filter out code that does not touch location x *)
            Some
              {
                array_name = Variable.name x;
                name = k.name;
                ranges = k.ranges;
                local_variables = k.local_variables;
                global_variables = k.global_variables;
                code = p;
              }
        | None -> None (* No locations being used, so ignore *))
end
(* ------------------------ THIRD STAGE OF TRANSLATION ---------------------- *)

let translate (stream : Phasesplit.Kernel.t Streamutil.stream) :
    Kernel.t Streamutil.stream =
  stream
  |> Streamutil.map Kernel.from_phased
  (* We have a stream of streams, flatten it *)
  |> Streamutil.concat

let print_kernels (ks : Kernel.t Streamutil.stream) : unit =
  print_endline "; locsplit";
  let count = ref 0 in
  Streamutil.iter
    (fun (k : Kernel.t) ->
      let curr = !count + 1 in
      count := curr;
      print_endline ("; loc " ^ string_of_int curr);
      Indent.print (Kernel.to_s k))
    ks;
  print_endline "; end of locsplit"
