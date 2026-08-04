open Protocols

type t = Ty.t Ty.Map.t

let empty : t = Ty.Map.empty

let rec resolve (ty : Ty.t) (db : t) : Ty.t =
  match Ty.Map.find_opt ty db with
  | Some ty -> ty
  | None ->
      Ty.Map.find_opt (Ty.strip_const ty) db
      |> Option.map Ty.add_const
      |> Option.value ~default:(Ty.map_children (fun ty -> resolve ty db) ty)

let add (x : Typedef.t) (db : t) : t =
  if Option.is_none (Ty.to_opaque x.alias) then db
  else
    let db = Ty.Map.add x.alias (resolve x.ty db) db in
    Ty.Map.map (fun ty -> resolve ty db) db

let to_string (db : t) : string =
  Ty.Map.bindings db
  |> List.map (fun (k, v) -> Ty.to_string k ^ " = " ^ Ty.to_string v)
  |> String.concat ", "
