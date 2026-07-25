open Stage0
open Common
open Exp

(* Pure-function registry: parallels [Predicates] but for entries
   whose body returns [nexp]. Two flavours of body coexist:

   - Algebraic rewrites ([divUp], [min], [max]) that lower to
     existing [nexp] shapes ([Binary], [NIf]) regardless of whether
     arguments are literal.

   - Uninterpreted-function bodies ([log2], [log], [sqrt], [__ffs],
     [__clz]) that default to [NCall name args] and concrete-fold
     to a [Num] when every argument is a [Num] literal. The Z3
     encoder (see [Gen_z3.n_to_expr]) treats matching [NCall] names
     as the same UF symbol, so two call sites with the same
     arguments share a value naturally. *)

type result =
  | Rewrites
  | Integer of (nexp list -> nexp -> bexp)
  | Not_representable

type t = { name : string; body : nexp list -> nexp; result : result }

let in_range (lo : int) (hi : int) : result =
  Integer (fun _ r -> b_and (n_le (Num lo) r) (n_le r (Num hi)))

(* Integer log2 floor: the largest [k] with [2^k <= n], for [n >= 1]. *)
let log2_floor (n : int) : int =
  if n < 1 then invalid_arg "log2_floor: requires n >= 1"
  else
    let rec aux acc n = if n <= 1 then acc else aux (acc + 1) (n lsr 1) in
    aux 0 n

(* Integer square root: the largest [k] with [k*k <= n], for [n >= 0]. *)
let isqrt (n : int) : int =
  if n < 0 then invalid_arg "isqrt: requires n >= 0"
  else int_of_float (Float.sqrt (Float.of_int n))

let all : t list =
  let div_up : t =
    { name = "divUp";
      body = (function
        | [ a; b ] ->
            let open Signedness in
            Binary (Div Signed,
                    Binary (Plus Signed, a, Binary (Minus Signed, b, Num 1)),
                    b)
        | _ -> failwith "divUp: expects exactly 2 arguments");
      result = Rewrites }
  in
  let min_fn : t =
    { name = "min";
      body = (function
        | [ a; b ] -> NIf (NRel (Lt Signedness.Signed, a, b), a, b)
        | _ -> failwith "min: expects exactly 2 arguments");
      result = Rewrites }
  in
  let max_fn : t =
    { name = "max";
      body = (function
        | [ a; b ] -> NIf (NRel (Gt Signedness.Signed, a, b), a, b)
        | _ -> failwith "max: expects exactly 2 arguments");
      result = Rewrites }
  in
  let log2_fn : t =
    { name = "log2";
      body = (function
        | [ Num k ] when k > 0 -> Num (log2_floor k)
        | args -> NCall ("log2", args));
      result = Not_representable }
  in
  let log_fn : t =
    (* [log] returns a real value; integer truncation depends on the
       caller's cast, so concrete-fold is intentionally absent. The
       UF default is enough for cross-call-site sharing. *)
    { name = "log";
      body = (fun args -> NCall ("log", args));
      result = Not_representable }
  in
  let sqrt_fn : t =
    { name = "sqrt";
      body = (function
        | [ Num k ] when k >= 0 ->
            let s = isqrt k in
            if s * s = k then Num s else NCall ("sqrt", [ Num k ])
        | args -> NCall ("sqrt", args));
      result = Not_representable }
  in
  let ffs : t =
    (* CUDA's [__ffs(x)] returns position of the lowest set bit + 1,
       or 0 when x = 0. *)
    { name = "__ffs";
      body = (function
        | [ Num 0 ] -> Num 0
        | [ Num k ] -> Num (log2_floor (k land -k) + 1)
        | args -> NCall ("__ffs", args));
      result = in_range 0 32 }
  in
  let clz : t =
    (* CUDA's [__clz(x)] returns the count of leading zeros in the
       32-bit unsigned representation. *)
    { name = "__clz";
      body = (function
        | [ Num 0 ] -> Num 32
        | [ Num k ] when k > 0 -> Num (31 - log2_floor k)
        | args -> NCall ("__clz", args));
      result = in_range 0 32 }
  in
  let umulhi : t =
    (* CUDA's [__umulhi(a, b)] is the high 32 bits of the 64-bit
       product. Its exact value is not derivable symbolically (nor
       useful for race checks), but as a pure function of its two
       arguments the UF default gives it cross-thread consistency:
       two threads passing equal arguments get the same result. This
       is what stops the reciprocal-multiply divide used by ggml's
       fastdiv from fabricating thread-divergent indices. *)
    { name = "__umulhi";
      body = (fun args -> NCall ("__umulhi", args));
      result = in_range 0 (Common.pow ~base:2 32 - 1) }
  in
  [ div_up; min_fn; max_fn;
    log2_fn; log_fn; sqrt_fn; ffs; clz; umulhi ]

let all_db : t StringMap.t =
  List.fold_left (fun m (e : t) -> StringMap.add e.name e m) StringMap.empty all

let call_opt (name : string) (args : nexp list) : nexp option =
  StringMap.find_opt name all_db |> Option.map (fun (e : t) -> e.body args)

let supported (name : string) : bool = StringMap.mem name all_db

let applications : bexp -> nexp list =
  let rec b_walk (acc : nexp list) (b : bexp) : nexp list =
    match b with
    | Bool _ -> acc
    | NRel (_, n1, n2) -> n_walk (n_walk acc n1) n2
    | BRel (_, b1, b2) -> b_walk (b_walk acc b1) b2
    | BNot b -> b_walk acc b
    | Pred (_, ns) | Distinct ns -> List.fold_left n_walk acc ns
    | CastBool n | IsThreadUnif n -> n_walk acc n
    | AtomicResult { index; operation; _ } ->
        let acc = List.fold_left n_walk acc index in
        Atomic.Operation.fold (fun n acc -> n_walk acc n) operation acc
  and n_walk (acc : nexp list) (n : nexp) : nexp list =
    match n with
    | Var _ | Num _ -> acc
    | Unary (_, e) -> n_walk acc e
    | Binary (_, n1, n2) -> n_walk (n_walk acc n1) n2
    | CastInt b -> b_walk acc b
    | NIf (b, n1, n2) -> n_walk (n_walk (b_walk acc b) n1) n2
    | NCall (_, args) -> List.fold_left n_walk (n :: acc) args
  in
  fun b -> b_walk [] b |> List.sort_uniq n_compare

let postcondition (n : nexp) : bexp option =
  match n with
  | NCall (name, args) -> (
      match StringMap.find_opt name all_db with
      | Some { result = Integer post; _ } -> Some (post args n)
      | Some { result = Rewrites | Not_representable; _ } | None -> None)
  | _ -> None

let add_postconditions (b : bexp) : bexp =
  applications b |> List.filter_map postcondition |> List.fold_left b_and b
