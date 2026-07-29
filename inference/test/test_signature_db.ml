open Protocols
open Inference
open D_lang

(* A function declared but never defined, and the same function defined,
   share a name and a type string, so they occupy the same slot in the
   database. Which one lands there decides whether a call to it is
   inlined or discarded, and the answer must not depend on the order the
   two declarations appear in. *)

let param (ty : string) (name : string) : C_lang.Param.t =
  C_lang.Param.make ~is_used:true ~is_shared:false
    ~ty_var:
      (Ty_variable.make ~ty:(Ty.of_c_string ty) ~name:(Variable.from_name name))

let kernel ~(name : string) ~(ty : string) ~(params : C_lang.Param.t list)
    ~(body : bool) : Kernel.t =
  {
    ty;
    name;
    code = (if body then Stmt.BreakStmt else Stmt.Skip);
    type_params = [];
    params;
    attribute = C_lang.KernelAttr.Auxiliary;
  }

let touch_ty = "void (int *, int)"

let touch ~(body : bool) : Kernel.t =
  kernel ~name:"touch" ~ty:touch_ty
    ~params:[ param "int *" "A"; param "int" "i" ]
    ~body

let definition = Def.Kernel (touch ~body:true)
let prototype = Def.Prototype (touch ~body:false)

let entry ?policy ~(name : string) ~(ty : string) (p : Program.t) :
    Kernel.t option =
  SignatureDB.from_program ?policy p
  |> SignatureDB.get ~kernel:name ~ty ~arg_count:2

(* The entry is a definition exactly when the program holds one. *)
let has_body ?policy (p : Program.t) : bool option =
  entry ?policy ~name:"touch" ~ty:touch_ty p
  |> Option.map (fun (k : Kernel.t) -> k.code <> Stmt.Skip)

let check ?policy (name : string) (expected : bool option) (p : Program.t) =
  ( name,
    `Quick,
    fun () -> Alcotest.(check (option bool)) name expected (has_body ?policy p)
  )

let ordering_tests =
  [
    check "a prototype alone registers" (Some false) [ prototype ];
    check "a definition alone registers" (Some true) [ definition ];
    check "declaration before definition keeps the definition" (Some true)
      [ prototype; definition ];
    (* [add] overwrites, so this is the order that used to lose the body. *)
    check "definition before re-declaration keeps the definition" (Some true)
      [ definition; prototype ];
  ]

(* Only a prototype the policy calls opaque is registered. Registering it
   is what makes a call to it reach [Imp] as an unresolvable one. *)

let scalar_proto =
  Def.Prototype
    (kernel ~name:"score" ~ty:"int (int)" ~params:[ param "int" "i" ]
       ~body:false)

(* [min] is modelled by the [Functions] registry, whose lowering is
   selected by this lookup missing, so no policy may register it. *)
let registry_proto =
  Def.Prototype
    (kernel ~name:"min" ~ty:"int (int *, int)"
       ~params:[ param "int *" "A"; param "int" "i" ]
       ~body:false)

let registered ?policy ~(name : string) ~(ty : string) (p : Program.t) : bool =
  entry ?policy ~name ~ty p |> Option.is_some

let check_registered ?policy (label : string) (expected : bool)
    ~(name : string) ~(ty : string) (p : Program.t) =
  ( label,
    `Quick,
    fun () ->
      Alcotest.(check bool) label expected (registered ?policy ~name ~ty p) )

let policy_tests =
  let open Opaque_call_policy in
  [
    check_registered ~policy:Skip_all "skip-all registers no prototype" false
      ~name:"touch" ~ty:touch_ty [ prototype ];
    check_registered ~policy:Skip_without_arrays
      "a writable parameter is opaque" true ~name:"touch" ~ty:touch_ty
      [ prototype ];
    check_registered ~policy:Skip_without_arrays
      "scalar parameters alone are not opaque" false ~name:"score"
      ~ty:"int (int)" [ scalar_proto ];
    check_registered ~policy:Skip_none "skip-none takes the scalar one too"
      true ~name:"score" ~ty:"int (int)" [ scalar_proto ];
    check_registered ~policy:Skip_none
      "a function the registry models is never opaque" false ~name:"min"
      ~ty:"int (int *, int)" [ registry_proto ];
  ]

let () =
  Alcotest.run "SignatureDB"
    [ ("definition wins", ordering_tests); ("opaque policy", policy_tests) ]
