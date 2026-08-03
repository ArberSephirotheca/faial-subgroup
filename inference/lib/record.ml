open Protocols
open Stage0

type t = {
  name : Ty.segment;
  qualifier : Ty.segment list;
  bases : Ty.segment list list;
  fields : (string * Ty.t) list;
  location : Location.t;
}

let location (r : t) : Location.t = r.location
let path (r : t) : Ty.segment list = r.qualifier @ [ r.name ]
let qualified_name (r : t) : string = Ty.to_string (Ty.named (path r))

let type_path (ty : Ty.t) : Ty.segment list option =
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
  let scopes (s : string) : Ty.segment list =
    if String.contains s '<' || String.contains s '(' then [ Ty.segment s ]
    else
      let n = String.length s in
      let rec split (start : int) (i : int) : string list =
        if i + 1 >= n then [ String.sub s start (n - start) ]
        else if s.[i] = ':' && s.[i + 1] = ':' then
          String.sub s start (i - start) :: split (i + 2) (i + 2)
        else split start (i + 1)
      in
      split 0 0 |> List.filter (fun s -> s <> "") |> List.map Ty.segment
  in
  match ty.inner with
  | Ty.Named path -> Some path
  | Ty.Struct { members = [] } -> Option.map (fun n -> scopes (strip n)) ty.name
  | Ty.Opaque s -> Some (scopes (strip s))
  | _ -> None

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
