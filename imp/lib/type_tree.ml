open Protocols

module type Resolver = sig
  val members : Ty.t -> (string * Ty.t) list option
end

module Leaf = struct
  type t = { path : Path.t; dims : int option list; ty : Ty.t }

  let name (x : t) : Variable.t = Path.to_variable x.path
  let arity (x : t) : int = List.length x.dims

  let to_string (x : t) : string =
    let dims =
      x.dims
      |> List.map (function Some n -> string_of_int n | None -> "?")
      |> String.concat ", "
    in
    Path.to_string x.path ^ " : " ^ Ty.to_string x.ty ^ " [" ^ dims ^ "]"
end

module Root = struct
  type t = { path : Path.t; ty : Ty.t }

  let to_string (x : t) : string =
    "*" ^ Path.to_string x.path ^ " : " ^ Ty.to_string x.ty
end

type t = { leaves : Leaf.t list; cuts : Root.t list }

let empty : t = { leaves = []; cuts = [] }

let union (a : t) (b : t) : t =
  { leaves = a.leaves @ b.leaves; cuts = a.cuts @ b.cuts }

let concat : t list -> t = List.fold_left union empty

let to_memory ~(hierarchy : Mem_hierarchy.t) (dims : int option list)
    (ty : Ty.t) : Memory.t =
  {
    hierarchy;
    size = dims;
    data_type =
      Ty.to_string ty |> String.split_on_char ' '
      |> List.filter (fun s -> String.length s > 0);
  }

let to_arrays ~(hierarchy : Mem_hierarchy.t) (x : t) :
    (Variable.t * Memory.t) list =
  (x.leaves
   |> List.map (fun (l : Leaf.t) ->
       (Leaf.name l, to_memory ~hierarchy l.dims l.ty)))
  @ (x.cuts
     |> List.map (fun (c : Root.t) ->
         ( Path.to_variable (Path.deref c.path),
           to_memory ~hierarchy [ None ] c.ty )))

(* A vector's lanes occupy disjoint bytes and are selected by name, so
   until a vector is a leaf in its own right (native vector support) they
   are products, which is what both front ends already do for a vector
   variable. *)
let lanes (ty : Ty.t) : (string * Ty.t) list option =
  match ty.inner with
  | Ty.Vector v ->
      Ty.vector_lanes ty
      |> Option.map (List.map (fun lane -> (lane, Ty.scalar v.scalar)))
  | _ -> None

module Make (R : Resolver) = struct
  let members (ty : Ty.t) : (string * Ty.t) list option =
    match R.members ty with Some m -> Some m | None -> lanes ty

  let rec descend ~(memory : bool) (path : Path.t) (dims : int option list)
      (ty : Ty.t) : t =
    match ty.inner with
    | Ty.Array { base; size } -> descend ~memory path (dims @ [ size ]) base
    | Ty.Reference ty -> descend ~memory path dims ty
    | _ -> (
        match members ty with
        | Some members ->
            members
            |> List.map (fun (name, mty) ->
                member ~memory (Path.select name path) dims mty)
            |> concat
        | None ->
            if memory then { leaves = [ { Leaf.path; dims; ty } ]; cuts = [] }
            else empty)

  (* A pointer member holds an address, so the region it names is not part
     of the object and the descent restarts there. Its own storage stays a
     leaf of the object it sits in. *)
  and member ~(memory : bool) (path : Path.t) (dims : int option list)
      (ty : Ty.t) : t =
    match ty.inner with
    | Ty.Pointer pointee ->
        {
          leaves = [ { Leaf.path; dims; ty } ];
          cuts = [ { Root.path; ty = pointee } ];
        }
    | _ -> descend ~memory path dims ty

  (* A pointer parameter names the array it points at, so the outermost
     dimension is the one the pointer supplies and its extent is unknown. *)
  let of_parameter ~(root : Variable.t) (ty : Ty.t) : t =
    let path = Path.root root in
    match ty.inner with
    | Ty.Pointer pointee -> descend ~memory:true path [ None ] pointee
    | Ty.Array _ -> descend ~memory:true path [] ty
    | _ -> descend ~memory:false path [] ty

  let of_declaration ?(dims = []) ~(root : Variable.t) (ty : Ty.t) : t =
    descend ~memory:true (Path.root root) dims ty
end
