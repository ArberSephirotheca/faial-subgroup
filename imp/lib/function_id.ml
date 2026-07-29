type t = {
  qualifier : string list;
  name : string;
  template_args : string list;
  ty : string;
}

let make ?(qualifier = []) ?(template_args = []) ~(name : string)
    ~(ty : string) () : t =
  { qualifier; name; template_args; ty }

(* A front end with no overloading, no namespaces and no templates
   needs only a name; WGSL is the case. *)
let of_name (name : string) : t =
  { qualifier = []; name; template_args = []; ty = name }

let name (x : t) : string = x.name
let ty (x : t) : string = x.ty

(* The qualified, template-applied name, without the type: what a
   reader of a diagnostic wants to see. *)
let label (x : t) : string =
  let args =
    if x.template_args = [] then ""
    else "<" ^ String.concat ", " x.template_args ^ ">"
  in
  String.concat "" (List.map (fun q -> q ^ "::") x.qualifier) ^ x.name ^ args

let to_string (x : t) : string = label x ^ ":" ^ x.ty

let compare (x : t) (y : t) : int =
  match String.compare x.name y.name with
  | 0 -> (
      match String.compare x.ty y.ty with
      | 0 -> (
          match List.compare String.compare x.qualifier y.qualifier with
          | 0 -> List.compare String.compare x.template_args y.template_args
          | n -> n)
      | n -> n)
  | n -> n

let equal (x : t) (y : t) : bool = compare x y = 0

module OT = struct
  type t' = t
  type t = t'

  let compare = compare
end

module Set = Set.Make (OT)
module Map = Map.Make (OT)
