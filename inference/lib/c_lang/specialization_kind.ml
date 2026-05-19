type t =
  | Undeclared
  | ImplicitInstantiation
  | ExplicitSpecialization
  | ExplicitInstantiationDeclaration
  | ExplicitInstantiationDefinition

let parse (s : string) : t option =
  match s with
  | "Undeclared" -> Some Undeclared
  | "ImplicitInstantiation" -> Some ImplicitInstantiation
  | "ExplicitSpecialization" -> Some ExplicitSpecialization
  | "ExplicitInstantiationDeclaration" ->
      Some ExplicitInstantiationDeclaration
  | "ExplicitInstantiationDefinition" ->
      Some ExplicitInstantiationDefinition
  | _ -> None

let to_string : t -> string = function
  | Undeclared -> "Undeclared"
  | ImplicitInstantiation -> "ImplicitInstantiation"
  | ExplicitSpecialization -> "ExplicitSpecialization"
  | ExplicitInstantiationDeclaration -> "ExplicitInstantiationDeclaration"
  | ExplicitInstantiationDefinition -> "ExplicitInstantiationDefinition"
