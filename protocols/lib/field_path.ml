open Stage0

module Step = struct
  type 'a t = { field : string; index : 'a list }
end

type 'a t = {
  root : Variable.t;
  index : 'a list;
  steps : 'a Step.t list;
  deref : bool;
}

let root (x : Variable.t) : 'a t =
  { root = x; index = []; steps = []; deref = false }

let parse (x : Variable.t) : 'a t =
  let n = Variable.name x in
  let deref = String.starts_with ~prefix:"*" n in
  let n = if deref then String.sub n 1 (String.length n - 1) else n in
  match String.split_on_char '.' n with
  | r :: fields ->
      {
        root = Variable.set_name r x;
        index = [];
        steps = List.map (fun field -> { Step.field; index = [] }) fields;
        deref;
      }
  | [] -> { root = x; index = []; steps = []; deref }

let base (p : 'a t) : Variable.t = p.root

let members (p : 'a t) : string list =
  List.map (fun (s : 'a Step.t) -> s.field) p.steps

let is_root (p : 'a t) : bool = p.steps = []

let select (field : string) (p : 'a t) : 'a t =
  { p with steps = p.steps @ [ { Step.field; index = [] } ] }

let deref (p : 'a t) : 'a t = { p with deref = true }
let is_deref (p : 'a t) : bool = p.deref

let selector (p : 'a t) : 'a list =
  p.index @ List.concat_map (fun (s : 'a Step.t) -> s.index) p.steps

let subscript (index : 'a list) (p : 'a t) : 'a t =
  match List.rev p.steps with
  | [] -> { p with index = p.index @ index }
  | last :: rest ->
      {
        p with
        steps =
          List.rev ({ last with Step.index = last.Step.index @ index } :: rest);
      }

let without_selector (p : 'a t) : 'a t =
  {
    p with
    index = [];
    steps = List.map (fun (s : 'a Step.t) -> { s with Step.index = [] }) p.steps;
  }

let map (f : 'a -> 'b) (p : 'a t) : 'b t =
  {
    root = p.root;
    index = List.map f p.index;
    steps =
      List.map
        (fun (s : 'a Step.t) ->
          { Step.field = s.field; index = List.map f s.index })
        p.steps;
    deref = p.deref;
  }

let map_state (f : 'a -> ('s, 'b) State.t) (p : 'a t) : ('s, 'b t) State.t =
  let open State in
  let open State.Syntax in
  let* index = list_map f p.index in
  let* steps =
    list_map
      (fun (s : 'a Step.t) ->
        let* index = list_map f s.index in
        return { Step.field = s.field; index })
      p.steps
  in
  return { root = p.root; index; steps; deref = p.deref }

let set_location (location : Location.t) (p : 'a t) : 'a t =
  { p with root = Variable.set_location location p.root }

let graft ~(prefix : 'a t) (p : 'a t) : 'a t =
  let index, steps =
    match List.rev prefix.steps with
    | [] -> (prefix.index @ p.index, p.steps)
    | last :: rest ->
        ( prefix.index,
          List.rev ({ last with Step.index = last.Step.index @ p.index } :: rest)
          @ p.steps )
  in
  { root = prefix.root; index; steps; deref = prefix.deref || p.deref }

let to_name (render : 'a -> string option) (p : 'a t) : string =
  let subscripts (n : string) (l : 'a list) : string =
    List.fold_left
      (fun n e -> match render e with Some s -> n ^ s | None -> n)
      n l
  in
  let n = subscripts (Variable.name p.root) p.index in
  let n =
    List.fold_left
      (fun n (s : 'a Step.t) -> subscripts (n ^ "." ^ s.field) s.index)
      n p.steps
  in
  if p.deref then "*" ^ n else n

let render_cell (e : Exp.nexp) : string option =
  match e with
  | Exp.Num 0 -> None
  | Exp.Num k -> Some ("[" ^ string_of_int k ^ "]")
  | _ -> Some "[?]"

let name (p : Exp.nexp t) : string = to_name render_cell p

let to_variable (p : Exp.nexp t) : Variable.t =
  if is_root p && (not p.deref) && selector p = [] then p.root
  else Variable.set_name (name p) p.root

let is_rooted_at (x : Variable.t) (p : 'a t) : bool = Variable.equal x p.root
let to_string (p : Exp.nexp t) : string = name p

let equal (p1 : 'a t) (p2 : 'a t) : bool =
  Variable.equal p1.root p2.root
  && members p1 = members p2
  && p1.deref = p2.deref
  && selector p1 = selector p2

let compare (p1 : Exp.nexp t) (p2 : Exp.nexp t) : int =
  String.compare (name p1) (name p2)

module Denotation = struct
  type t = One_region | Many_regions
end

let denotation (p : Exp.nexp t) : Denotation.t =
  if List.for_all (function Exp.Num _ -> true | _ -> false) (selector p) then
    One_region
  else Many_regions

let under ~(root : Variable.t) (p : 'a t) : 'a t option =
  let target = parse root in
  let rec split (fields : string list) (steps : 'a Step.t list) :
      ('a list * 'a Step.t list) option =
    match (fields, steps) with
    | [], steps -> Some ([], steps)
    | f :: fields, (s : 'a Step.t) :: steps when String.equal f s.field ->
        split fields steps
        |> Option.map (fun (index, rest) -> (s.index @ index, rest))
    | _ :: _, _ -> None
  in
  if Variable.equal target.root p.root && ((not target.deref) || p.deref) then
    split (members target) p.steps
    |> Option.map (fun (index, steps) ->
           {
             root = Variable.set_name (Variable.name root) p.root;
             index = p.index @ index;
             steps;
             deref = p.deref && not target.deref;
           })
  else None
