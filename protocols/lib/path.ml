type t = { root : Variable.t; members : string list }

let root (root : Variable.t) : t = { root; members = [] }
let base (p : t) : Variable.t = p.root
let members (p : t) : string list = p.members
let is_root (p : t) : bool = p.members = []
let select (member : string) (p : t) : t = { p with members = p.members @ [ member ] }

let graft ~(prefix : t) (p : t) : t =
  { root = prefix.root; members = prefix.members @ p.members }

let name (p : t) : string =
  List.fold_left (fun n m -> n ^ "." ^ m) (Variable.name p.root) p.members

let to_variable (p : t) : Variable.t =
  if is_root p then p.root else Variable.set_name (name p) p.root

let of_variable (root : Variable.t) : t = { root; members = [] }
let is_rooted_at (x : Variable.t) (p : t) : bool = Variable.equal x p.root
let to_string (p : t) : string = name p
let equal (p1 : t) (p2 : t) : bool = Variable.equal p1.root p2.root && p1.members = p2.members
let compare (p1 : t) (p2 : t) : int = String.compare (name p1) (name p2)

(* Recover the path a mangled name encodes, under a root we already hold.
   This decodes rather than constructs, which is sound here and only here:
   the separator is minted by faial from field identifiers, and neither a
   field name nor a variable name can contain one. It is not a way to read
   a name the front end did not build.

   It exists because a pointer binds a root while an access names a member
   of it, so [Atom *q = s; q->f[j]] has to match [q] against [q.f]. *)
let under ~(root : Variable.t) (p : t) : t option =
  let r = Variable.name root and n = Variable.name p.root in
  if String.equal r n then Some p
  else
    let prefix = r ^ "." in
    if String.starts_with ~prefix n then
      let rest =
        String.sub n (String.length prefix) (String.length n - String.length prefix)
      in
      Some
        {
          root = Variable.set_name r p.root;
          members = String.split_on_char '.' rest @ p.members;
        }
    else None
