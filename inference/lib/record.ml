open Protocols
open Stage0

type t = {
  name : string;
  qualifier : string list;
  template_args : string list;
  bases : string list;
  fields : (string * Ty.t) list;
  location : Location.t;
}

let location (r : t) : Location.t = r.location

let qualified_name (r : t) : string =
  J_type.specialization ~name:r.name ~qualifier:r.qualifier
    ~args:r.template_args

let type_name (ty : Ty.t) : string option =
  let prefixes =
    List.map (fun (spelling, _) -> spelling ^ " ") Qualifier.spellings
    @ [ "struct "; "class "; "union " ]
  in
  let rec strip (s : string) : string =
    match
      List.find_opt (fun prefix -> String.starts_with ~prefix s) prefixes
    with
    | Some prefix ->
        strip
          (String.sub s (String.length prefix)
             (String.length s - String.length prefix))
    | None -> s
  in
  match ty.inner with
  | Ty.Struct { members = [] } -> Option.map strip ty.name
  | Ty.Opaque s -> Some (strip s)
  | _ -> None

let pattern_name (s : string) : string option =
  let n = String.length s in
  if n = 0 || s.[n - 1] <> '>' then None
  else
    let rec opening (i : int) (depth : int) : string option =
      if i < 0 then None
      else
        match s.[i] with
        | '>' -> opening (i - 1) (depth + 1)
        | '<' when depth = 1 -> Some (String.sub s 0 i)
        | '<' -> opening (i - 1) (depth - 1)
        | _ -> opening (i - 1) depth
    in
    opening (n - 1) 0

let to_ty (r : t) : Ty.t =
  Ty.make ~name:(qualified_name r) (Ty.Struct { members = r.fields })

let to_string (r : t) : string =
  let fields =
    r.fields
    |> List.map (fun (n, ty) -> Ty.to_string ty ^ " " ^ n)
    |> String.concat "; "
  in
  "struct " ^ qualified_name r ^ " { " ^ fields ^ " }"

let to_s (r : t) : Indent.t list = [ Line (to_string r ^ ";") ]
