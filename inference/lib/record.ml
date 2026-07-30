open Protocols
open Stage0

type t = {
  name : string;
  fields : (string * Ty.t) list;
  location : Location.t;
}

let location (r : t) : Location.t = r.location

let type_name (ty : Ty.t) : string option =
  let strip (s : string) : string =
    List.fold_left
      (fun s prefix ->
        if String.starts_with ~prefix s then
          String.sub s (String.length prefix)
            (String.length s - String.length prefix)
        else s)
      s
      [ "struct "; "class "; "union " ]
  in
  match ty.inner with
  | Ty.Struct { members = [] } -> Option.map strip ty.name
  | Ty.Opaque s -> Some (strip s)
  | _ -> None

let to_ty (r : t) : Ty.t =
  Ty.make ~name:r.name (Ty.Struct { members = r.fields })

let to_string (r : t) : string =
  let fields =
    r.fields
    |> List.map (fun (n, ty) -> Ty.to_string ty ^ " " ^ n)
    |> String.concat "; "
  in
  "struct " ^ r.name ^ " { " ^ fields ^ " }"

let to_s (r : t) : Indent.t list = [ Line (to_string r ^ ";") ]
