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

type t = {
  name : string;
  arity : int;
  body : nexp list -> nexp;
  result : result;
}

let in_range (lo : int) (hi : int) : result =
  if lo > hi then
    invalid_arg
      ("in_range: empty interval " ^ string_of_int lo ^ ".." ^ string_of_int hi)
  else Integer (fun _ r -> b_and (n_le (Num lo) r) (n_le r (Num hi)))

(* The range of a [w]-bit unsigned result. A [Num] holds an OCaml
   [int], so a width at or above [Sys.int_size] has no upper bound to
   name: computing one overflows, which swaps the ends of the range,
   and a range whose ends are swapped is a contradiction that
   discharges every goal it reaches as data-race free. Raising here
   fails while the registry is being built. *)
let unsigned_bits (w : int) : result =
  if w >= Sys.int_size then
    invalid_arg
      ("unsigned_bits: a " ^ string_of_int w ^ "-bit range does not fit in Num")
  else in_range 0 (pow ~base:2 w - 1)

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

(* Count of set bits, for [n >= 0]. *)
let popcount (n : int) : int =
  if n < 0 then invalid_arg "popcount: requires n >= 0"
  else
    let rec aux acc n = if n = 0 then acc else aux (acc + (n land 1)) (n lsr 1) in
    aux 0 n

(* The operand a 32-bit intrinsic actually reads, so that folding a
   literal wider than 32 bits agrees with the hardware's truncation
   instead of reporting a count no execution can produce. *)
let low32 (n : int) : int = n land 0xFFFFFFFF

let all : t list =
  let div_up : t =
    { name = "divUp";
      arity = 2;
      body = (function
        | [ a; b ] -> n_div (n_plus a (n_minus b (Num 1))) b
        | _ -> failwith "divUp: expects exactly 2 arguments");
      result = Rewrites }
  in
  let min_fn : t =
    { name = "min";
      arity = 2;
      body = (function
        | [ a; b ] -> n_if (n_lt a b) a b
        | _ -> failwith "min: expects exactly 2 arguments");
      result = Rewrites }
  in
  let max_fn : t =
    { name = "max";
      arity = 2;
      body = (function
        | [ a; b ] -> n_if (n_gt a b) a b
        | _ -> failwith "max: expects exactly 2 arguments");
      result = Rewrites }
  in
  let log2_fn : t =
    { name = "log2";
      arity = 1;
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
      arity = 1;
      body = (fun args -> NCall ("log", args));
      result = Not_representable }
  in
  let sqrt_fn : t =
    { name = "sqrt";
      arity = 1;
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
      arity = 1;
      body = (function
        | [ Num k ] ->
            let k = low32 k in
            Num (if k = 0 then 0 else log2_floor (k land -k) + 1)
        | args -> NCall ("__ffs", args));
      result = in_range 0 32 }
  in
  let ffsll : t =
    (* The 64-bit twin of [__ffs]. Its operand is wider than a [Num]
       can hold, so folding is confined to a non-negative literal,
       where the two agree. *)
    { name = "__ffsll";
      arity = 1;
      body = (function
        | [ Num 0 ] -> Num 0
        | [ Num k ] when k > 0 -> Num (log2_floor (k land -k) + 1)
        | args -> NCall ("__ffsll", args));
      result = in_range 0 64 }
  in
  let clz : t =
    (* CUDA's [__clz(x)] returns the count of leading zeros in the
       32-bit unsigned representation. *)
    { name = "__clz";
      arity = 1;
      body = (function
        | [ Num k ] ->
            let k = low32 k in
            Num (if k = 0 then 32 else 31 - log2_floor k)
        | args -> NCall ("__clz", args));
      result = in_range 0 32 }
  in
  let clzll : t =
    { name = "__clzll";
      arity = 1;
      body = (function
        | [ Num 0 ] -> Num 64
        | [ Num k ] when k > 0 -> Num (63 - log2_floor k)
        | args -> NCall ("__clzll", args));
      result = in_range 0 64 }
  in
  let popc : t =
    (* CUDA's [__popc(x)] counts the set bits of a 32-bit value. *)
    { name = "__popc";
      arity = 1;
      body = (function
        | [ Num k ] -> Num (popcount (low32 k))
        | args -> NCall ("__popc", args));
      result = in_range 0 32 }
  in
  let popcll : t =
    { name = "__popcll";
      arity = 1;
      body = (function
        | [ Num k ] when k >= 0 -> Num (popcount k)
        | args -> NCall ("__popcll", args));
      result = in_range 0 64 }
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
      arity = 2;
      body = (fun args -> NCall ("__umulhi", args));
      result = unsigned_bits 32 }
  in
  [ div_up; min_fn; max_fn;
    log2_fn; log_fn; sqrt_fn;
    ffs; ffsll; clz; clzll; popc; popcll; umulhi ]

let all_db : t StringMap.t =
  List.fold_left (fun m (e : t) -> StringMap.add e.name e m) StringMap.empty all

(* An entry only governs an application of its own arity. A name
   reused at another arity is a different function, and both the body
   and the postcondition would be wrong about it. *)
let find_opt (name : string) (args : nexp list) : t option =
  match StringMap.find_opt name all_db with
  | Some e when e.arity = List.length args -> Some e
  | Some _ | None -> None

(* What an application should become. A [Rewrites] entry always lowers
   to its body, which is the function's graph and so holds for
   symbolic arguments too. Every other entry keeps its symbol unless
   each argument is a literal, in which case the body folds it away. *)
let call_opt (name : string) (args : nexp list) : nexp option =
  match find_opt name args with
  | Some { result = Rewrites; body; _ } -> Some (body args)
  | Some { body; _ } ->
      if List.for_all (function Num _ -> true | _ -> false) args then
        Some (body args)
      else None
  | None -> None

let supported (name : string) : bool = StringMap.mem name all_db

let postcondition (n : nexp) : bexp option =
  match n with
  | NCall (name, args) -> (
      match find_opt name args with
      | Some { result = Integer post; _ } -> Some (post args n)
      | Some { result = Rewrites | Not_representable; _ } | None -> None)
  | _ -> None

let add_postconditions (b : bexp) : bexp =
  b_calls b |> List.filter_map postcondition |> List.fold_left b_and b
