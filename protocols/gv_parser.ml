open Stage0

type t = {
  pass : bool;
  block_dim : Dim3.t;
  grid_dim : Dim3.t;
  options : string list;
}

let default_grid_dim : Dim3.t = Dim3.{ x = 1; y = 1; z = 1 }
let default_block_dim : Dim3.t = Dim3.{ x = 1024; y = 1; z = 1 }

let default =
  {
    pass = true;
    block_dim = default_block_dim;
    grid_dim = default_grid_dim;
    options = [];
  }

let make ?(block_dim = default_block_dim) ?(grid_dim = default_grid_dim)
    ?(pass = true) ?(options = []) () : t =
  { block_dim; grid_dim; pass; options }

let to_string (x : t) : string =
  "{pass="
  ^ (if x.pass then "true" else "false")
  ^ ", block_dim=" ^ Dim3.to_string x.block_dim ^ ", grid_dim="
  ^ Dim3.to_string x.grid_dim ^ ", options=["
  ^ String.concat ", " x.options
  ^ "]}"

let serialize (x : t) : string =
  let dim3 (x : Dim3.t) : string =
    x |> Dim3.to_list |> List.map string_of_int |> String.concat "," |> fun x ->
    "[" ^ x ^ "]"
  in
  (if x.pass then "//pass\n" else "//xfail:BOOGIE_ERROR\n")
  ^ "//--blockDim=" ^ dim3 x.block_dim ^ " " ^ "--gridDim=" ^ dim3 x.grid_dim
  ^ "\n"

let to_assoc (x : t) : (string * int) list =
  Dim3.to_assoc ~prefix:"blockDim." x.block_dim
  @ Dim3.to_assoc ~prefix:"gridDim." x.grid_dim

let parse_line (l : string) : string list option =
  let len = String.length l in
  if len >= 2 && String.starts_with ~prefix:"//" l then
    String.sub l 2 (len - 2)
    |> String.trim |> String.split_on_char ' ' |> Option.some
  else None

let parse_pass (header : string) : bool option =
  if String.starts_with ~prefix:"//" header then
    let header = header |> String.trim in
    match Slice.from_start 2 |> Slice.substring header |> String.trim with
    | "pass" -> Some true
    | "xfail:BOOGIE_ERROR" -> Some false
    | _ -> None
  else None

let parse_params (l : string list) (self : t) : t =
  let offset ~prefix:(sep : string) (part : string) : string option =
    if String.starts_with ~prefix:sep part then
      let len = String.length sep in
      Some (String.sub part len (String.length part - len))
    else None
  in
  List.fold_left
    (fun (self : t) (part : string) : t ->
      match offset ~prefix:"--blockDim=" part with
      | Some block_dim ->
          { self with block_dim = Dim3.parse block_dim |> Result.get_ok }
      | None -> (
          match offset ~prefix:"--gridDim=" part with
          | Some grid_dim ->
              { self with grid_dim = Dim3.parse grid_dim |> Result.get_ok }
          | None -> (
              match offset ~prefix:"-D" part with
              | Some opt -> { self with options = self.options @ [ opt ] }
              | None ->
                  prerr_endline ("Gv_parser.parse: skipping parameter: " ^ part);
                  self)))
    self l

let from_pair ~pass ~params : t option =
  let ( let* ) = Option.bind in
  let* pass = parse_pass pass in
  let* params = parse_line params in
  Some (parse_params params { default with pass })

let from_string (x : string) : t option =
  match String.split_on_char '\n' x with
  | pass :: params :: _ -> from_pair ~pass ~params
  | _ -> None

let parse (filename : string) : t option =
  let ic = open_in filename in
  let pass = input_line ic in
  let params = input_line ic in
  close_in ic;
  from_pair ~pass ~params
