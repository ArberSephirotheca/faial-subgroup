open Stage0
open Protocols
open Exp
module VarSet = Variable.Set
module VarMap = Variable.Map
module StringSet = Common.StringSet

(* ----------------- constants -------------------- *)
let thread_globals : VarSet.t =
  List.map Variable.from_name
    [
      "blockDim.x";
      "blockIdx.x";
      "gridDim.x";
      "gridIdx.x";
      "threadDim.x";
      "blockDim.y";
      "blockIdx.y";
      "gridDim.y";
      "gridIdx.y";
      "threadDim.y";
      "blockDim.z";
      "blockIdx.z";
      "gridDim.z";
      "gridIdx.z";
      "threadDim.z";
      "warpSize";
    ]
  |> VarSet.of_list

let known_types : StringSet.t =
  [
    "__builtin_va_list";
    "__float128";
    "__int128_t";
    "__uint128_t";
    "bool";
    "char";
    "char16_t";
    "char32_t";
    "cudaError_t";
    "cudaStream";
    "cudaStream_t";
    "cudaTextureObject_t";
    "curandDiscreteDistribution_st";
    "curandDiscreteDistribution_t";
    "curandState";
    "curandStateMRG32k3a";
    "curandStateMRG32k3a_t";
    "curandStateMtgp32";
    "curandStateMtgp32_t";
    "curandStatePhilox4_32_10";
    "curandStatePhilox4_32_10_t";
    "curandStateScrambledSobol32";
    "curandStateScrambledSobol32_t";
    "curandStateScrambledSobol64";
    "curandStateScrambledSobol64_t";
    "curandStateSobol32";
    "curandStateSobol32_t";
    "curandStateSobol64";
    "curandStateSobol64_t";
    "curandStateXORWOW";
    "curandStateXORWOW_t";
    "curandState_t";
    "dim4";
    "double";
    "enumcudaError";
    "float";
    "int";
    "int16_t";
    "int32_t";
    "int64_t";
    "int8_t";
    "long";
    "ptrdiff_t";
    "short";
    "size_t";
    "uint";
    "uint16_t";
    "uint32_t";
    "uint8_t";
    "ushort";
    "wchar_t";
    "void";
  ]
  |> StringSet.of_list

let vector_types : StringSet.t =
  [
    "char";
    "double";
    "float";
    "int";
    "long";
    "longlong";
    "short";
    "uchar";
    "uint";
    "ulong";
    "ulonglong";
    "ushort";
  ]
  |> StringSet.of_list

(* ----------------- serialization -------------------- *)

(* Converts an array index to a string *)
let idx_to_s (f : 'a -> string) (l : 'a list) : string =
  "[" ^ String.concat "][" (List.map f l) ^ "]"

(* Gives the dummy variable string for any variable *)
let var_to_dummy (v : Variable.t) : string = "__dummy" ^ Variable.name v

(* Checks if a variable is a dummy variable *)
let is_dummy_var (v : Variable.t) : bool =
  String.starts_with ~prefix:"__dummy" (Variable.name v)

(* Gives the dummy instruction for any array read/write *)
let acc_expr_to_dummy (a : Access.t) : Indent.t list =
  let x = a.array in
  let var = Variable.name x ^ idx_to_s n_to_string a.Access.index in
  let open Access in
  match a.mode with
  | Read -> [ Line (var_to_dummy x ^ " = " ^ var ^ ";") ]
  | Write None -> [ Line (var ^ " = " ^ var_to_dummy x ^ "_w();") ]
  | Write (Some i) -> [ Line (var ^ " = " ^ string_of_int i ^ ";") ]
  | Atomic _ -> failwith "acc_expr_to_dummy: Atomic not supported"

(* Converts a division loop to a multiplication loop *)
let div_to_mult (r : Range.t) : Range.t =
  match (r.step, r.dir) with
  | Mult _, Decrease -> { r with dir = Increase }
  | _ -> r

(* Converts a loop increment to a string *)
let inc_to_s (r : Range.t) : string =
  Variable.name r.var
  ^
  match (r.step, r.dir) with
  | Plus step, Increase -> " += " ^ n_to_string step
  | Plus step, Decrease -> " -= " ^ n_to_string step
  | Mult step, Increase -> " *= " ^ n_to_string step
  | Mult step, Decrease -> " /= " ^ n_to_string step

(* Converts source instruction to a valid CUDA operation *)
let rec inst_to_s (g : Generator.t) : Code.t -> Indent.t list = function
  | Access e -> acc_expr_to_dummy e
  | Sync s ->
      if Protocols.Sync.is_syncthreads s then [ Line "__syncthreads();" ]
      else
        let mnem = Protocols.Sync.Mode.to_string s.mode in
        let operand_exprs : string list =
          n_to_string s.id
          :: (match s.participants with
              | Some c -> [ n_to_string c ]
              | None -> [])
        in
        let template =
          operand_exprs
          |> List.mapi (fun i _ -> Printf.sprintf "%%%d" i)
          |> String.concat ", "
        in
        let operand_list =
          operand_exprs
          |> List.map (fun e -> Printf.sprintf "\"r\"(%s)" e)
          |> String.concat ", "
        in
        [
          Line
            (Printf.sprintf "asm volatile(\"bar.%s %s;\" :: %s);" mnem template
               operand_list);
        ]
  | If (b, p, q) ->
      [
        Line ("if (" ^ b_to_string b ^ ") {");
        Block (inst_to_s g p);
        Line "} else {";
        Block (inst_to_s g q);
        Line "}";
      ]
  | Skip -> []
  | Decl { var; ty; body = p; pre = _ } ->
      (* Synthesised Decls are uninitialised scaffold variables — strip
         [const] so the emitted C++ doesn't fail "default initialisation
         of an object of const type". Clang dependent-type placeholders
         (e.g. "<dependent type>" inside templated bodies) leak through
         as type strings; substitute [int] so the file remains parseable. *)
      let ty_s = C_type.to_string (C_type.strip_const ty) in
      let ty_s =
        if String.length ty_s > 0 && ty_s.[0] = '<' then "int" else ty_s
      in
      Line (ty_s ^ " " ^ Variable.name var ^ ";")
      :: inst_to_s g p
  | Seq (p, q) -> inst_to_s g p @ inst_to_s g q
  | Loop { range = r; body = p } ->
      let x = Variable.name r.var in
      let r = if g.div_to_mult then div_to_mult r else r in
      let lb, ub, op =
        match r.dir with
        | Increase -> (r.lower_bound, r.upper_bound, " <= ")
        | Decrease -> (n_dec r.upper_bound, n_dec r.lower_bound, " >= ")
      in
      [
        Line
          ("for (" ^ "int " ^ x ^ " = " ^ n_to_string lb ^ "; " ^ x ^ op
         ^ n_to_string ub ^ "; " ^ inc_to_s r ^ ") {");
        Block (inst_to_s g p);
        Line "}";
      ]

(* Checks if a string is a known CUDA type *)
let is_known_type (s : string) : bool =
  if StringSet.mem s known_types then true
  else
    match String.length s with
    | 0 -> true
    (* Checks if a string is a vector type *)
    | len -> (
        StringSet.mem (String.sub s 0 (len - 1)) vector_types
        && match s.[len - 1] with '1' | '2' | '3' | '4' -> true | _ -> false)

(* Creates empty class declarations for unknown types *)
let decl_unknown_types (vm : Memory.t VarMap.t) : string list =
  (* Converts an array type to a declaration, or None if it is a known type *)
  let rec arr_type_to_decl : string list -> string option = function
    | [] -> None
    | [ last_type ] ->
        if is_known_type last_type then None
        else Some ("class " ^ last_type ^ " {};")
    | _ :: types -> arr_type_to_decl types
  in
  VarMap.bindings vm
  |> List.filter_map (fun (_, v) -> arr_type_to_decl v.Memory.data_type)
  (* Remove duplicates *)
  |> StringSet.of_list
  |> StringSet.elements

(* Gets the type of an array, defaulting to int if it is unknown *)
let arr_type ?(strip_const = false) (arr : Memory.t) : string =
  if arr.data_type = [] then "int"
  else if strip_const then
    List.filter (fun x -> x <> "const") arr.data_type |> String.concat " "
  else String.concat " " arr.data_type

(* Create external function prototypes for writing to each array *)
let arr_to_proto (vm : Memory.t VarMap.t) (g : Generator.t) : string list =
  let modifiers =
    "extern "
    ^ if g.expand_device then "__attribute__((device)) " else "__device__ "
  in
  VarMap.bindings vm
  |> List.map (fun (k, v) ->
      modifiers ^ arr_type v ~strip_const:true ^ " " ^ var_to_dummy k ^ "_w();")

(* Helper functions for making kernel parameters *)
let global_arr_to_l (vm : Memory.t VarMap.t) : string list =
  VarMap.bindings vm
  |> List.map (fun (k, v) -> arr_type v ^ " *" ^ Variable.name k)

let global_var_to_l (vs : VarSet.t) : string list =
  VarSet.elements (VarSet.diff vs thread_globals)
  |> List.map (fun v -> "int " ^ Variable.name v)

(* Helper functions for making kernel variable declarations *)
let arr_to_shared (vm : Memory.t VarMap.t) : Indent.t list =
  VarMap.bindings vm
  |> List.map (fun (k, v) ->
      Indent.Line
        ((if v.Memory.size = [] then "extern " else "")
        ^ "__shared__ " ^ arr_type v ^ " " ^ Variable.name k
        ^ idx_to_s string_of_int v.Memory.size
        ^ ";"))

let local_var_to_l (vs : VarSet.t) (g : Generator.t) : Indent.t list =
  (* Use a single dummy array/function to initialize all local variables *)
  let init_local_var (v : Variable.t) : Indent.t =
    let rhs =
      if g.use_dummy_array then "__dummy[threadIdx.x]" else "__dummy_int()"
    in
    Indent.Line ("int " ^ Variable.name v ^ " = " ^ rhs ^ ";")
  in
  (* Architectural names are CUDA builtins — never declare them.
     [thread_globals] covers blockIdx/blockDim/gridDim/warpSize; [is_tid]
     covers threadIdx, which is intentionally not in [thread_globals]. *)
  VarSet.diff vs thread_globals
  |> VarSet.filter (fun v -> not (Variable.is_tid v || is_dummy_var v))
  |> VarSet.elements |> List.map init_local_var

let arr_to_dummy (vm : Memory.t VarMap.t) : Indent.t list =
  VarMap.bindings vm
  |> List.map (fun (k, v) ->
      Indent.Line (arr_type v ~strip_const:true ^ " " ^ var_to_dummy k ^ ";"))

(* Serialization of the kernel header *)
let header_to_s (g : Generator.t) (gv : Gv_parser.t) (k : Kernel.t) : Indent.t =
  let comments =
    if g.gen_params && not g.toml then [ Gv_parser.serialize gv ] else []
  in
  let type_decls = decl_unknown_types k.arrays in
  let base_protos =
    if g.use_dummy_array then []
    else if g.expand_device then
      [ "extern __attribute__((device)) int __dummy_int();" ]
    else [ "extern __device__ int __dummy_int();" ]
  in
  let funct_protos = base_protos @ arr_to_proto k.arrays g in
  Indent.Line (comments @ type_decls @ funct_protos |> String.concat "\n")

(* Serialization of the kernel body *)
let body_to_s (f : Code.t -> Indent.t list) (g : Generator.t) (k : Kernel.t) :
    Indent.t =
  let shared_arr =
    k.arrays |> VarMap.filter (fun _ -> Memory.is_shared) |> arr_to_shared
  in
  let local_var = local_var_to_l (Params.to_set k.local_variables) g in
  let dummy_var = arr_to_dummy k.arrays in
  Indent.Block (shared_arr @ local_var @ dummy_var @ f k.code)

(* Serialization of the kernel *)
let kernel_to_s (f : Code.t -> Indent.t list) (g : Generator.t)
    (gv : Gv_parser.t) (k : Kernel.t) : Indent.t list =
  let base_params = if g.use_dummy_array then [ "int *__dummy" ] else [] in
  let global_arr =
    k.arrays |> VarMap.filter (fun _ -> Memory.is_global) |> global_arr_to_l
  in
  let global_var = global_var_to_l (Params.to_set k.global_variables) in
  let params = base_params @ global_arr @ global_var |> String.concat ", " in
  [
    header_to_s g gv k;
    Line ("__global__ void " ^ k.name ^ "(" ^ params ^ ")");
    Line "{";
    body_to_s f g k;
    Line "}";
  ]

let prog_to_s (g : Generator.t) (p : Code.t) : Indent.t list = inst_to_s g p

(* Inference synthesizes intermediate names containing '@' (e.g.
   "@AccessState98"). '@' is not a valid character in a C identifier,
   so we sanitize generated output by prefixing every '@'-name with a
   stable, collision-resistant prefix. *)
let synth_prefix : string = "__synth_"

let sanitize_at (s : string) : string =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if c = '@' then Buffer.add_string buf synth_prefix
      else Buffer.add_char buf c)
    s;
  Buffer.contents buf

let gen_cuda (g : Generator.t) (gv : Gv_parser.t) (k : Kernel.t) : string =
  kernel_to_s (prog_to_s g) g gv k |> Indent.to_string |> sanitize_at

(* Serialization of RaCUDA parameters *)
let gen_params (gv : Gv_parser.t) : string =
  let dim_to_s (d : Dim3.t) : string =
    let x, y, z = (string_of_int d.x, string_of_int d.y, string_of_int d.z) in
    x ^ " " ^ y ^ " " ^ z ^ "\n"
  in
  "blockDim " ^ dim_to_s gv.block_dim ^ "blockIdx 0 0 0\n" ^ "gridDim "
  ^ dim_to_s gv.grid_dim ^ "initThread 0 0 0\n"
