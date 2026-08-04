open Stage0

module Step = struct
  type 'a t = { field : string; deref : bool; index : 'a list }
end

type 'a t = { root : Variable.t; index : 'a list; steps : 'a Step.t list }

let root (x : Variable.t) : 'a t = { root = x; index = []; steps = [] }
let base (p : 'a t) : Variable.t = p.root

let members (p : 'a t) : string list =
  List.map (fun (s : 'a Step.t) -> s.field) p.steps

let is_root (p : 'a t) : bool = p.steps = []
let is_deref (p : 'a t) : bool = List.exists (fun (s : 'a Step.t) -> s.deref) p.steps

let select (field : string) (p : 'a t) : 'a t =
  { p with steps = p.steps @ [ { Step.field; deref = false; index = [] } ] }

let map_last (f : 'a Step.t -> 'a Step.t) (p : 'a t) : 'a t =
  match List.rev p.steps with
  | [] -> p
  | last :: rest -> { p with steps = List.rev (f last :: rest) }

let deref (p : 'a t) : 'a t = map_last (fun s -> { s with Step.deref = true }) p

let subscript (index : 'a list) (p : 'a t) : 'a t =
  if p.steps = [] then { p with index = p.index @ index }
  else map_last (fun s -> { s with Step.index = s.Step.index @ index }) p

let split (p : 'a t) : 'a list * 'a list =
  let below (steps : 'a Step.t list) : 'a list =
    List.concat_map (fun (s : 'a Step.t) -> s.Step.index) steps
  in
  let rec walk (before : 'a list) : 'a Step.t list -> 'a list * 'a list =
    function
    | [] -> (before, [])
    | (s : 'a Step.t) :: steps when s.deref -> (before, below (s :: steps))
    | s :: steps -> walk (before @ s.Step.index) steps
  in
  if is_deref p then walk p.index p.steps else ([], p.index @ below p.steps)

let region_index (p : 'a t) : 'a list = snd (split p)

let subscripts (p : 'a t) : 'a list =
  let selector, region = split p in
  selector @ region

let without_subscripts (p : 'a t) : 'a t =
  {
    p with
    index = [];
    steps = List.map (fun (s : 'a Step.t) -> { s with Step.index = [] }) p.steps;
  }

let without_region_index (p : 'a t) : 'a t =
  let rec walk : 'a Step.t list -> 'a Step.t list = function
    | [] -> []
    | (s : 'a Step.t) :: steps when s.deref ->
        { s with Step.index = [] }
        :: List.map (fun (s : 'a Step.t) -> { s with Step.index = [] }) steps
    | s :: steps -> s :: walk steps
  in
  if is_deref p then { p with steps = walk p.steps } else without_subscripts p

let crossings (p : 'a t) : ('a t * 'a list) list * 'a list =
  let rec walk (above : 'a Step.t list) (index : 'a list)
      (acc : ('a t * 'a list) list) :
      'a Step.t list -> ('a t * 'a list) list * 'a list = function
    | [] -> (List.rev acc, index)
    | (s : 'a Step.t) :: steps ->
        let above = above @ [ { s with Step.index = [] } ] in
        if s.deref then
          let storage =
            map_last
              (fun s -> { s with Step.deref = false })
              { p with index = []; steps = above }
          in
          walk above s.index ((storage, index) :: acc) steps
        else walk above (index @ s.Step.index) acc steps
  in
  walk [] p.index [] p.steps

let map (f : 'a -> 'b) (p : 'a t) : 'b t =
  {
    root = p.root;
    index = List.map f p.index;
    steps =
      List.map
        (fun (s : 'a Step.t) ->
          { Step.field = s.field; deref = s.deref; index = List.map f s.index })
        p.steps;
  }

let map_state (f : 'a -> ('s, 'b) State.t) (p : 'a t) : ('s, 'b t) State.t =
  let open State in
  let open State.Syntax in
  let* index = list_map f p.index in
  let* steps =
    list_map
      (fun (s : 'a Step.t) ->
        let* index = list_map f s.index in
        return { Step.field = s.field; deref = s.deref; index })
      p.steps
  in
  return { root = p.root; index; steps }

let set_location (location : Location.t) (p : 'a t) : 'a t =
  { p with root = Variable.set_location location p.root }

let graft ~(prefix : 'a t) (p : 'a t) : 'a t =
  if prefix.steps = [] then
    { p with root = prefix.root; index = prefix.index @ p.index }
  else
    {
      root = prefix.root;
      index = prefix.index;
      steps = (subscript p.index prefix).steps @ p.steps;
    }

let to_name (render : 'a -> string option) (p : 'a t) : string =
  let subscripts (n : string) (l : 'a list) : string =
    List.fold_left
      (fun n e -> match render e with Some s -> n ^ s | None -> n)
      n l
  in
  let rec walk (n : string) (crossed : bool) : 'a Step.t list -> string =
    function
    | [] -> n
    | (s : 'a Step.t) :: steps ->
        let n = subscripts (n ^ (if crossed then "->" else ".") ^ s.field) s.index in
        walk n s.deref steps
  in
  let n = walk (subscripts (Variable.name p.root) p.index) false p.steps in
  match List.rev p.steps with
  | (last : 'a Step.t) :: _ when last.deref -> "*" ^ n
  | _ -> n

let render_cell (e : Exp.nexp) : string option =
  match e with
  | Exp.Num 0 -> None
  | Exp.Num k -> Some ("[" ^ string_of_int k ^ "]")
  | _ -> Some "[?]"

let name (p : Exp.nexp t) : string = to_name render_cell p

let parse (x : Variable.t) : 'a t =
  let n = Variable.name x in
  let deref_last = String.starts_with ~prefix:"*" n in
  let n = if deref_last then String.sub n 1 (String.length n - 1) else n in
  let arrows (s : string) : string list =
    let len = String.length s in
    let rec walk (start : int) (i : int) (acc : string list) : string list =
      if i + 1 < len && s.[i] = '-' && s.[i + 1] = '>' then
        walk (i + 2) (i + 2) (String.sub s start (i - start) :: acc)
      else if i >= len then List.rev (String.sub s start (len - start) :: acc)
      else walk start (i + 1) acc
    in
    walk 0 0 []
  in
  match String.split_on_char '.' n with
  | r :: fields ->
      let steps =
        fields
        |> List.concat_map (fun field ->
               match arrows field with
               | first :: rest ->
                   { Step.field = first; deref = rest <> []; index = [] }
                   :: List.mapi
                        (fun i field ->
                          {
                            Step.field;
                            deref = i < List.length rest - 1;
                            index = [];
                          })
                        rest
               | [] -> [])
      in
      let steps =
        if deref_last then
          match List.rev steps with
          | last :: rest -> List.rev ({ last with Step.deref = true } :: rest)
          | [] -> steps
        else steps
      in
      let root = if steps = [] then x else Variable.set_name r x in
      { root; index = []; steps }
  | [] -> root x

let to_variable (p : Exp.nexp t) : Variable.t =
  if is_root p && p.index = [] then p.root
  else Variable.set_name (name p) p.root

let to_string (p : Exp.nexp t) : string = name p

let under ~(root : Variable.t) (p : 'a t) : 'a t option =
  let target = parse root in
  let rec split (target : 'a Step.t list) (steps : 'a Step.t list) :
      ('a list * bool * 'a Step.t list) option =
    match (target, steps) with
    | [], steps -> Some ([], false, steps)
    | (t : 'a Step.t) :: target, (s : 'a Step.t) :: steps
      when String.equal t.field s.field && ((not t.deref) || s.deref) ->
        split target steps
        |> Option.map (fun (index, crossed, rest) ->
               (s.Step.index @ index, crossed || (s.deref && not t.deref), rest))
    | _ :: _, _ -> None
  in
  if Variable.equal target.root p.root then
    split target.steps p.steps
    |> Option.map (fun (index, crossed, steps) ->
           let name = Variable.name root in
           {
             root = Variable.set_name (if crossed then "*" ^ name else name) p.root;
             index = p.index @ index;
             steps;
           })
  else None
