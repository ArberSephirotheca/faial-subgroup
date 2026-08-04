open Protocols

(* A field of a record, with where the declaration side says it sits. *)
module Field = struct
  type t = { name : string; ty : Ty.t; offset : int option }

  let make ?(offset : int option) ~(name : string) ~(ty : Ty.t) () : t =
    { name; ty; offset }
end

module type Resolver = sig
  val members : Ty.t -> Field.t list option

  (* The width of a record, which only the declaration side knows. *)
  val size : Ty.t -> int option
end

module Leaf = struct
  type t = {
    path : Exp.nexp Field_path.t;
    dims : int option list;
    ty : Ty.t;
    layout : Memory.Layout.t option;
  }

  let name (x : t) : Variable.t = Field_path.to_variable x.path
  let arity (x : t) : int = List.length x.dims

  let to_string (x : t) : string =
    let dims =
      x.dims
      |> List.map (function Some n -> string_of_int n | None -> "?")
      |> String.concat ", "
    in
    Field_path.to_string x.path ^ " : " ^ Ty.to_string x.ty ^ " [" ^ dims ^ "]"
end

type t = { leaves : Leaf.t list }

let empty : t = { leaves = [] }
let union (a : t) (b : t) : t = { leaves = a.leaves @ b.leaves }

let concat : t list -> t = List.fold_left union empty

let to_memory ~(hierarchy : Mem_hierarchy.t) ?(layout : Memory.Layout.t option)
    (dims : int option list) (ty : Ty.t) : Memory.t =
  {
    hierarchy;
    size = dims;
    data_type =
      Ty.to_string ty |> String.split_on_char ' '
      |> List.filter (fun s -> String.length s > 0);
    layout;
  }

let to_arrays ~(hierarchy : Mem_hierarchy.t) (x : t) :
    (Variable.t * Memory.t) list =
  x.leaves
  |> List.map (fun (l : Leaf.t) ->
         (Leaf.name l, to_memory ~hierarchy ?layout:l.layout l.dims l.ty))

let to_region ~(hierarchy : Mem_hierarchy.t) (x : t) :
    ((Variable.t * Memory.t) * (Variable.t * Pointer.t) list) option =
  let ( let* ) = Option.bind in
  match x.leaves with
  | [] | [ _ ] -> None
  | first :: rest ->
      let root = Field_path.base first.path in
      let* layout = first.layout in
      let* width = Ty.width first.ty in
      let* stride =
        match List.rev layout.strides with s :: _ -> Some s | [] -> None
      in
      let alike (l : Leaf.t) : bool =
        Variable.equal (Field_path.base l.path) root
        && l.dims = first.dims
        && Ty.width l.ty = Some width
        && match l.layout with
           | Some m -> m.strides = layout.strides
           | None -> false
      in
      let offset (l : Leaf.t) : int option =
        let* m = l.layout in
        if m.offset mod width = 0 then Some (m.offset / width) else None
      in
      let* lanes =
        if width > 0 && stride mod width = 0 && stride / width > 1 then
          Some (stride / width)
        else None
      in
      if
        (not (List.for_all alike rest))
        || not (List.for_all Option.is_some (List.tl first.dims))
      then None
      else
        let lane_of = List.filter_map offset x.leaves in
        if List.length lane_of <> List.length x.leaves then None
        else
        let region =
          ( root,
            to_memory ~hierarchy (first.dims @ [ Some lanes ]) first.ty )
        in
        let scale = List.map (fun s -> s / width) layout.strides in
        let views =
          List.map2
            (fun (l : Leaf.t) (k : int) ->
              ( Leaf.name l,
                Pointer.linear ~scale ~shift:(Exp.Num k)
                  (Pointer.from_array root) ))
            x.leaves lane_of
        in
        Some (region, views)

(* A vector's lanes occupy disjoint bytes and are selected by name, so
   until a vector is a leaf in its own right (native vector support) they
   are products, which is what both front ends already do for a vector
   variable. *)
let lanes (ty : Ty.t) : Field.t list option =
  match ty.inner with
  | Ty.Vector v ->
      let width = Scalar.sizeof v.scalar in
      Ty.vector_lanes ty
      |> Option.map
           (List.mapi (fun i lane ->
                Field.make ~offset:(i * width * 8) ~name:lane
                  ~ty:(Ty.scalar v.scalar) ()))
  | _ -> None

module Make (R : Resolver) = struct
  let members (ty : Ty.t) : Field.t list option =
    match R.members ty with Some m -> Some m | None -> lanes ty

  let width (ty : Ty.t) : int option =
    match Ty.width ty with Some w -> Some w | None -> R.size ty

  let step (strides : int list option) (w : int option) : int list option =
    match (strides, w) with
    | Some strides, Some w -> Some (strides @ [ w ])
    | _ -> None

  let rec descend ~(memory : bool) ~(crossed : Ty.t list)
      ~(offset : int option) ~(strides : int list option)
      (path : Exp.nexp Field_path.t) (dims : int option list) (ty : Ty.t) : t =
    match ty.inner with
    | Ty.Array { base; size } ->
        descend ~memory ~crossed ~offset
          ~strides:(step strides (width base))
          path (dims @ [ size ]) base
    | Ty.Reference ty -> descend ~memory ~crossed ~offset ~strides path dims ty
    | _ -> (
        match members ty with
        | Some members ->
            members
            |> List.map (fun (f : Field.t) ->
                let offset =
                  match (offset, f.offset) with
                  | Some o, Some p -> Some (o + (p / 8))
                  | _ -> None
                in
                member ~memory ~crossed ~offset ~strides
                  (Field_path.select f.name path) dims f.ty)
            |> concat
        | None ->
            if memory then
              let layout =
                match (offset, strides) with
                | Some offset, Some strides
                  when List.length strides = List.length dims ->
                    Some (Memory.Layout.make ~offset ~strides)
                | _ -> None
              in
              { leaves = [ { Leaf.path; dims; ty; layout } ] }
            else empty)

  (* A pointer member holds an address, so the region it names is not part
     of the object: its own storage stays a leaf of the object it sits in,
     and the descent restarts below the address it holds. The region is
     indexed by that address and then by the offset the pointer supplies,
     which is why two extents lead the pointee's own. A pointee type
     already crossed on the way down repeats a region that is registered
     already, which is what stops a self-referential record. *)
  and member ~(memory : bool) ~(crossed : Ty.t list) ~(offset : int option)
      ~(strides : int list option) (path : Exp.nexp Field_path.t)
      (dims : int option list) (ty : Ty.t) : t =
    match ty.inner with
    | Ty.Pointer pointee ->
        let layout =
          match (offset, strides) with
          | Some offset, Some strides
            when List.length strides = List.length dims ->
              Some (Memory.Layout.make ~offset ~strides)
          | _ -> None
        in
        let storage = { leaves = [ { Leaf.path; dims; ty; layout } ] } in
        if List.exists (Ty.equal pointee) crossed then storage
        else
          union storage
            (* What an address points at is not part of the object that
               held it, so the layout restarts below the crossing. *)
            (descend ~memory:true ~crossed:(pointee :: crossed) ~offset:(Some 0)
               ~strides:(step (step (Some []) (width pointee)) (width pointee))
               (Field_path.deref path) [ None; None ] pointee)
    | _ -> descend ~memory ~crossed ~offset ~strides path dims ty

  (* A pointer parameter names the array it points at, so the outermost
     dimension is the one the pointer supplies and its extent is unknown. *)
  let of_parameter ~(root : Variable.t) (ty : Ty.t) : t =
    let path = Field_path.root root in
    let descend = descend ~crossed:[] ~offset:(Some 0) in
    match ty.inner with
    | Ty.Pointer pointee ->
        descend ~memory:true ~strides:(step (Some []) (width pointee)) path
          [ None ] pointee
    | Ty.Array _ -> descend ~memory:true ~strides:(Some []) path [] ty
    | _ -> descend ~memory:false ~strides:(Some []) path [] ty

  let of_declaration ?(dims = []) ~(root : Variable.t) (ty : Ty.t) : t =
    descend ~memory:true ~crossed:[] ~offset:(Some 0)
      ~strides:(if dims = [] then Some [] else None)
      (Field_path.root root) dims ty
end
