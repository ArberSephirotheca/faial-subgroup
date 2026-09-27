open Protocols
open Inference
open D_lang


let param (ty : string) (name : string) : C_lang.Param.t =
  C_lang.Param.make ~is_used:true ~is_shared:false
    ~ty_var:
      (Ty_variable.make ~ty:(Ty.of_c_string ty) ~name:(Variable.from_name name))

let id ?(qualifier = []) ?(template_args = []) ~(name : string)
    ~(ty : string) () : Imp.Function_id.t =
  Imp.Function_id.make ~qualifier ~template_args ~name ~ty ()

let kernel ?(qualifier = []) ?(template_args = []) ?decl_id ~(name : string)
    ~(ty : string) ~(params : C_lang.Param.t list) ~(body : bool) () :
    Kernel.t =
  {
    id = id ~qualifier ~template_args ~name ~ty ();
    decl_id;
    code = (if body then Stmt.BreakStmt else Stmt.Skip);
    type_params = [];
    template_args = [];
    params;
    attribute = C_lang.KernelAttr.Auxiliary;
    returns_location = false;
  }

let touch_ty = "void (int *, int)"

let touch ?decl_id ~(body : bool) () : Kernel.t =
  kernel ?decl_id ~name:"touch" ~ty:touch_ty
    ~params:[ param "int *" "A"; param "int" "i" ]
    ~body ()

let definition = Def.Kernel (touch ~decl_id:"def" ~body:true ())
let prototype = Def.Prototype (touch ~decl_id:"proto" ~body:false ())

let entry ?policy ?(qualifier = []) ?(template_args = []) ~(name : string)
    ~(ty : string) (p : Program.t) () : Kernel.t option =
  SignatureDB.from_program ?policy p
  |> SignatureDB.get_id (id ~qualifier ~template_args ~name ~ty ())

(* The entry is a definition exactly when the program holds one. *)
let has_body ?policy (p : Program.t) : bool option =
  entry ?policy ~name:"touch" ~ty:touch_ty p ()
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
       ~body:false ())

(* [min] is modelled by the [Functions] registry, whose lowering is
   selected by this lookup missing, so no policy may register it. *)
let registry_proto =
  Def.Prototype
    (kernel ~name:"min" ~ty:"int (int *, int)"
       ~params:[ param "int *" "A"; param "int" "i" ]
       ~body:false ())

let registered ?policy ~(name : string) ~(ty : string) (p : Program.t) : bool =
  entry ?policy ~name ~ty p () |> Option.is_some

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

let f_ty = "void (int *)"

let instance ~(arg : string) ~(decl_id : string) : Def.t =
  Def.Kernel
    (kernel ~template_args:[ arg ] ~decl_id ~name:"f" ~ty:f_ty
       ~params:[ param "int *" "A" ] ~body:true ())

let in_namespace ~(ns : string) ~(decl_id : string) : Def.t =
  Def.Kernel
    (kernel ~qualifier:[ Ty.segment ns ] ~decl_id ~name:"f" ~ty:f_ty
       ~params:[ param "int *" "A" ] ~body:true ())

let call_site (decl_id : string) : Expr.t =
  Ident
    {
      name = Variable.from_name "f";
      ty = Ty.of_c_string f_ty;
      kind = Decl_expr.Kind.Function;
      decl_id = Some decl_id; qualifier = [];
    }

let resolves (label : string) (p : Program.t) (decl_id : string)
    (expected : Imp.Function_id.t) =
  ( label,
    `Quick,
    fun () ->
      let found =
        SignatureDB.from_program p
        |> SignatureDB.lookup (call_site decl_id) 1
        |> Option.map (fun (s : SignatureDB.Signature.t) -> s.id)
      in
      Alcotest.(check (option string))
        label
        (Some (Imp.Function_id.to_string expected))
        (Option.map Imp.Function_id.to_string found) )

let distinct (label : string) (p : Program.t) (n : int) =
  ( label,
    `Quick,
    fun () ->
      let db = SignatureDB.from_program p in
      let count =
        p
        |> List.filter_map (function
             | Def.Kernel k -> SignatureDB.get_id k.Kernel.id db
             | _ -> None)
        |> List.length
      in
      Alcotest.(check int) label n count )

let identity_tests =
  let templates =
    [ instance ~arg:"0" ~decl_id:"i0"; instance ~arg:"1" ~decl_id:"i1" ]
  in
  let namespaces =
    [ in_namespace ~ns:"a" ~decl_id:"na"; in_namespace ~ns:"b" ~decl_id:"nb" ]
  in
  [
    distinct "two instantiations are two entries" templates 2;
    resolves "a call reaches the instantiation it names" templates "i0"
      (id ~template_args:[ "0" ] ~name:"f" ~ty:f_ty ());
    resolves "and not its sibling" templates "i1"
      (id ~template_args:[ "1" ] ~name:"f" ~ty:f_ty ());
    distinct "two namespaces are two entries" namespaces 2;
    resolves "a call reaches the namespace it names" namespaces "na"
      (id ~qualifier:[ Ty.segment "a" ] ~name:"f" ~ty:f_ty ());
    resolves "and not the other namespace" namespaces "nb"
      (id ~qualifier:[ Ty.segment "b" ] ~name:"f" ~ty:f_ty ());
  ]

let () =
  Alcotest.run "SignatureDB"
    [
      ("definition wins", ordering_tests);
      ("opaque policy", policy_tests);
      ("identity", identity_tests);
    ]
