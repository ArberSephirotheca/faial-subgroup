open Stage0
module StringMap = Common.StringMap
module StringSet = Common.StringSet

type t = {
  kernels : Kernel.t StringMap.t; (* Kernel name to kernel *)
  targets : StringSet.t StringMap.t;
      (* For each kernel which other kernels it is calling *)
  visited : StringSet.t;
}

let key_set (s : 'a StringMap.t) : StringSet.t =
  s |> StringMap.bindings |> List.map fst |> StringSet.of_list

let to_string (s : t) : string =
  let string_set (s : StringSet.t) =
    "[" ^ (StringSet.elements s |> String.concat ", ") ^ "]"
  in
  "{\n" ^ "\tkernels = "
  ^ (key_set s.kernels |> string_set)
  ^ "\n" ^ "\ttargets = "
  ^ String.concat ", "
      (s.targets |> StringMap.bindings
      |> List.map (fun (k, v) -> k ^ "=" ^ string_set v))
  ^ "\n" ^ "\tvisited = " ^ string_set s.visited ^ "\n" ^ "}"

let inline_kernels (kernels : StringSet.t) (s : t) : t =
  (* Get the code of the kernels to call *)
  let leaves =
    StringMap.filter (fun k _ -> StringSet.mem k kernels) s.kernels
  in
  let leaf_set = key_set leaves in
  (* Get the set of all kernels that call `kernel` *)
  let to_inline : StringSet.t =
    s.targets
    |> StringMap.filter (fun _ x ->
           (* any kernel that depends on a leaf *)
           not (StringSet.is_empty (StringSet.inter leaf_set x)))
    |> key_set
  in
  {
    (* inline each call to a leaf *)
    kernels =
      StringMap.mapi
        (fun name k ->
          (* if this kernel calls any of the leaves *)
          if StringSet.mem name to_inline then
            (* Inline leaves in k *)
            Kernel.inline leaves k
          else
            (* nothing to do, leave kernel as is *)
            k)
        s.kernels;
    (* remove the leaves from all dependencies *)
    targets = StringMap.map (fun s -> StringSet.diff s leaf_set) s.targets;
    (* add leaves to the set of all visited *)
    visited = StringSet.union leaf_set s.visited;
  }

(* Calculate the set of next possible kernels to inline *)
let next (s : t) : StringSet.t =
  let possible =
    s.targets |> StringMap.filter (fun _ ts -> StringSet.is_empty ts) |> key_set
  in
  StringSet.diff possible s.visited

let from_list (ks : Kernel.t list) : t =
  {
    targets =
      ks
      |> List.map (fun k -> (Kernel.unique_id k, Kernel.calls k))
      |> StringMap.of_list;
    kernels =
      ks |> List.map (fun k -> (Kernel.unique_id k, k)) |> StringMap.of_list;
    visited = StringSet.empty;
  }

let kernel_list (s : t) : Kernel.t list =
  s.kernels |> StringMap.bindings |> List.map snd

let rec inline_all (s : t) : t =
  let n = next s in
  if StringSet.is_empty n then
    (* we are done *)
    s
  else
    (* inline more *)
    inline_all (inline_kernels n s)

let inline_calls (l : Kernel.t list) : Kernel.t list =
  l |> from_list |> inline_all |> kernel_list
