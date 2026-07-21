(*
 Given a location-split kernel, generate a flat kernel.

 A flat-kernel has the following characteristics:
 - free from control-flow structures
 - all binders are hoisted
 - all concurrent accesses are available
 *)
open Stage0
open Protocols
open Exp

module CondAccess = struct
  type t = { access : Access.t; cond : bexp }

  let add_cond (b : bexp) (c : t) : t = { c with cond = b_and b c.cond }
  let dim (a : t) : int = List.length a.access.index
  let location (x : t) : Location.t = Access.location x.access
  let access (x : t) : Access.t = x.access

  let to_s (a : t) : Indent.t list =
    let lineno =
      a |> location |> Location.line |> Index.to_base1 |> string_of_int
    in
    let lineno = lineno ^ ": " in
    [
      Line (lineno ^ Access.to_string a.access ^ " if");
      Block (b_to_s a.cond);
      Line ";";
    ]

  let to_string (a : t) : string = to_s a |> Indent.to_string
end

module Code = struct
  type t = CondAccess.t list

  let to_list : t -> CondAccess.t list = fun x -> x
  let to_s (l : t) : Indent.t list = List.concat_map CondAccess.to_s l

  (* The dimention is the index count *)
  let dim (l : t) : int option = List.nth_opt l 0 |> Option.map CondAccess.dim

  let from_unsync : Unsynced.t -> t =
    let rec flatten (accum : t) (b : bexp) : Unsynced.t -> t = function
      | Skip -> accum
      | Assert _ ->
          failwith "Internall error: call Unsynced.inline_asserts first!"
      | Access e -> { access = e; cond = b } :: accum
      | Cond (b', p) -> flatten accum (b_and b' b) p
      | Loop (nr, p) -> flatten accum (b_and (Norm_range.to_bexp nr) b) p
      | Seq (p, q) ->
          let accum = flatten accum b p in
          flatten accum b q
    in
    fun u -> flatten [] (Bool true) (Unsynced.inline_asserts u)
end

module Kernel = struct
  type t = {
    name : string;
    array_name : string;
    approx_local_variables : Variable.Set.t;
    exact_local_variables : Variable.Set.t;
    code : Code.t;
    pre : bexp;
    runtime : bexp;
  }

  let to_s (k : t) : Indent.t list =
    [
      Line ("array: " ^ k.array_name ^ ";");
      Line
        ("exact locals: " ^ Variable.set_to_string k.exact_local_variables ^ ";");
      Line
        ("approx locals: "
        ^ Variable.set_to_string k.approx_local_variables
        ^ ";");
      Line ("pre: " ^ b_to_string k.pre ^ ";");
      Line ("rt: " ^ b_to_string k.runtime ^ ";");
      Line "{";
      Block (Code.to_s k.code);
      Line "}";
    ]

  let from_loc_split (arch : Architecture.t) (k : Locsplit.Kernel.t) : t option
      =
    (* Loop normalization. Reparametrize every strided additive loop,
       hoisted ([k.ranges]) and still-nested ([k.code]), over a fresh
       unit-stride index, substituting the recovered value for the
       original iteration variable everywhere it appears (see
       [Norm_range]). The hoisted substitutions are applied to the
       body before its own nested loops are normalized. *)
    let normalized_ranges = List.map Norm_range.normalize k.ranges in
    let subst_pairs =
      List.filter_map
        (fun nr ->
          match nr with
          | Norm_range.Index ix -> Some (Norm_range.substitution ix)
          | Norm_range.Plain _ -> None)
        normalized_ranges
    in
    (* Reparametrize outer hoisted counters in each range's bounds and
       restriction; each range's own counter is already reparametrized. *)
    let normalized_ranges =
      List.map
        (fun nr ->
          List.fold_left
            (fun nr sp -> Norm_range.map (Subst.ReplacePair.n_subst sp) nr)
            nr subst_pairs)
        normalized_ranges
    in
    let k_code =
      List.fold_left (fun c sp -> Unsynced.subst sp c) k.code subst_pairs
      |> Unsynced.normalize_loops
    in
    let code = Code.from_unsync k_code in
    if code = [] then None
    else
      let ids =
        (match arch with
          | Grid -> Variable.bid_set
          | Block -> Variable.Set.empty)
        |> Variable.Set.union Variable.tid_set
      in
      let approx_old =
        let with_ranges =
          List.fold_left
            (fun acc (r : Norm_range.t) ->
              let r_vars = Norm_range.free_names r Variable.Set.empty in
              if Variable.Set.is_empty (Variable.Set.inter r_vars acc) then acc
              else Variable.Set.add (Norm_range.var r) acc)
            (Params.to_set k.local_variables)
            normalized_ranges
        in
        let from_ranges = Variable.Set.diff with_ranges ids in
        let from_code =
          Variable.Set.diff (Params.to_set k.local_variables) ids
          |> Unsynced.unsafe_binders k_code
        in
        Variable.Set.union from_ranges from_code
      in
      let all_locals =
        approx_old
        |> Variable.Set.union (Unsynced.binders k_code Variable.Set.empty)
        |> Variable.Set.union ids
      in
      let pre = b_and_ex (List.map Norm_range.to_bexp normalized_ranges) in
      let constraints =
        b_and_split pre
        @ List.concat_map
            (fun (ca : CondAccess.t) -> b_and_split ca.cond)
            code
      in
      let ground (imprecise : Variable.Set.t) : Variable.Set.t =
        List.fold_left
          (fun imprecise c ->
            let imp =
              Variable.Set.inter (b_free_names c Variable.Set.empty) imprecise
            in
            match Variable.Set.elements imp with
            | [ v ] -> Variable.Set.remove v imprecise
            | _ -> imprecise)
          imprecise constraints
      in
      let rec fixpoint (imprecise : Variable.Set.t) : Variable.Set.t =
        let next = ground imprecise in
        if Variable.Set.equal next imprecise then imprecise else fixpoint next
      in
      let approx_local_variables = fixpoint approx_old in
      let exact_local_variables =
        Variable.Set.diff all_locals approx_local_variables
      in
      Some
        {
          name = k.name;
          array_name = k.array_name;
          code;
          exact_local_variables;
          approx_local_variables;
          pre;
          runtime =
            (* merge all parameters *)
            k.global_variables
            |> Params.union_left k.local_variables
            (* only retain the parameters that are used *)
            |> Params.retain_all (Locsplit.Kernel.free_names k)
            (* generate data type constraints *)
            |> Params.to_bexp;
        }
end

let translate (arch : Architecture.t)
    (stream : Locsplit.Kernel.t Streamutil.stream) : Kernel.t Streamutil.stream
    =
  let open Streamutil in
  filter_map (Kernel.from_loc_split arch) stream

(* ------------------- SERIALIZE ---------------------- *)

let print_kernels (ks : Kernel.t Streamutil.stream) : unit =
  print_endline "; flatacc";
  let count = ref 0 in
  Streamutil.iter
    (fun (k : Kernel.t) ->
      let curr = !count + 1 in
      count := curr;
      print_endline ("; acc " ^ string_of_int curr);
      Indent.print (Kernel.to_s k))
    ks;
  print_endline "; end of flatacc"
