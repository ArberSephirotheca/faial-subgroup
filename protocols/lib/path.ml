type t = {
  root : Variable.t;
  members : string list;
  deref : bool;
  selector : Exp.nexp list;
}

let of_variable (x : Variable.t) : t =
  let n = Variable.name x in
  if String.starts_with ~prefix:"*" n then
    {
      root = Variable.set_name (String.sub n 1 (String.length n - 1)) x;
      members = [];
      deref = true;
      selector = [];
    }
  else { root = x; members = []; deref = false; selector = [] }

let root : Variable.t -> t = of_variable
let base (p : t) : Variable.t = p.root
let members (p : t) : string list = p.members
let is_root (p : t) : bool = p.members = []
let select (member : string) (p : t) : t = { p with members = p.members @ [ member ] }
let deref (p : t) : t = { p with deref = true }
let is_deref (p : t) : bool = p.deref
let selector (p : t) : Exp.nexp list = p.selector
let with_selector (selector : Exp.nexp list) (p : t) : t = { p with selector }

module Denotation = struct
  type t = One_region | Many_regions
end

let denotation (p : t) : Denotation.t =
  if List.for_all (function Exp.Num _ -> true | _ -> false) p.selector then
    One_region
  else Many_regions

let map_selector (f : Exp.nexp -> Exp.nexp) (p : t) : t =
  { p with selector = List.map f p.selector }

let graft ~(prefix : t) (p : t) : t =
  {
    root = prefix.root;
    members = prefix.members @ p.members;
    deref = prefix.deref || p.deref;
    selector = prefix.selector @ p.selector;
  }

let without_selector (p : t) : t = { p with selector = [] }

let name (p : t) : string =
  let root =
    List.fold_left
      (fun n (e : Exp.nexp) ->
        match e with
        | Exp.Num 0 -> n
        | Exp.Num k -> n ^ "[" ^ string_of_int k ^ "]"
        | _ -> n ^ "[?]")
      (Variable.name p.root) p.selector
  in
  let n = List.fold_left (fun n m -> n ^ "." ^ m) root p.members in
  if p.deref then "*" ^ n else n

let to_variable (p : t) : Variable.t =
  if is_root p && not p.deref && p.selector = [] then p.root
  else Variable.set_name (name p) p.root

let is_rooted_at (x : Variable.t) (p : t) : bool = Variable.equal x p.root
let to_string (p : t) : string = name p

let equal (p1 : t) (p2 : t) : bool =
  Variable.equal p1.root p2.root && p1.members = p2.members
  && p1.deref = p2.deref && p1.selector = p2.selector

let compare (p1 : t) (p2 : t) : int = String.compare (name p1) (name p2)

let under ~(root : Variable.t) (p : t) : t option =
  let target = of_variable root in
  let r = Variable.name target.root and n = Variable.name p.root in
  if target.deref then if p.deref && String.equal r n then Some p else None
  else if String.equal r n then Some p
  else
    let prefix = r ^ "." in
    if String.starts_with ~prefix n then
      let rest =
        String.sub n (String.length prefix) (String.length n - String.length prefix)
      in
      Some
        {
          p with
          root = Variable.set_name r p.root;
          members = String.split_on_char '.' rest @ p.members;
        }
    else None
