open Protocols
open Vectors

type t = { config : Config.t; cond : Exp.bexp; env : NMap.t Variable.Map.t }

let to_string (ctx : t) : string =
  let env =
    ctx.env |> Variable.Map.bindings
    |> List.map (fun (x, y) -> "  " ^ Variable.name x ^ "=" ^ NMap.to_string y)
    |> String.concat "\n"
  in
  "cond: " ^ Exp.b_to_string ctx.cond ^ "\nenv:\n" ^ env

let make (config : Config.t) : t =
  { cond = Exp.Bool true; env = Variable.Map.empty; config }

let restrict (b : Exp.bexp) (ctx : t) : t =
  let open Exp in
  { ctx with cond = b_and ctx.cond b }

let put (x : Variable.t) (v : NMap.t) (ctx : t) : t =
  { ctx with env = Variable.Map.add x v ctx.env }

let get (x : Variable.t) (ctx : t) : NMap.t option =
  Variable.Map.find_opt x ctx.env

let zero_cost (ctx : t) : NMap.t =
  NMap.constant ~count:ctx.config.bank_count ~value:0

let put_tids (block_dim : Dim3.t) (ctx : t) : t =
  let wids = NMap.make ctx.config.threads_per_warp (fun x -> x) in
  let n_tidx = NMap.map (fun id -> id mod block_dim.x) wids in
  let n_tidy = NMap.map (fun id -> id / block_dim.x mod block_dim.y) wids in
  let n_tidz =
    NMap.map (fun id -> id / (block_dim.x * block_dim.y) mod block_dim.z) wids
  in
  ctx |> put Variable.tid_x n_tidx |> put Variable.tid_y n_tidy
  |> put Variable.tid_z n_tidz

let tid_opt (ctx : t) : Dim3.t array option =
  let ( let* ) = Option.bind in
  let* tid_x = get Variable.tid_x ctx |> Option.map NMap.to_array in
  let* tid_y = get Variable.tid_y ctx |> Option.map NMap.to_array in
  let* tid_z = get Variable.tid_z ctx |> Option.map NMap.to_array in
  let tid_xy = Array.combine tid_x tid_y in
  let tid_xyz = Array.combine tid_xy tid_z in
  Some (tid_xyz |> Array.map (fun ((x, y), z) -> Dim3.{ x; y; z }))

let tid (ctx : t) : Dim3.t array = tid_opt ctx |> Option.get

let from_config (params : Config.t) : t =
  make params |> put_tids params.block_dim

let ( let* ) = Result.bind

let rec n_eval_res (n : Exp.nexp) (ctx : t) : (NMap.t, string) Result.t =
  match n with
  | Var x -> (
      match Variable.Map.find_opt x ctx.env with
      | Some x -> Ok x
      | None -> Error ("undefined variable: " ^ Variable.name x))
  | Num n -> Ok (NMap.constant ~count:ctx.config.threads_per_warp ~value:n)
  | CastInt (CastBool n) -> n_eval_res n ctx
  | CastInt e ->
      let* e = b_eval_res e ctx in
      Ok
        (e |> BMap.to_array
        |> Array.map (fun v -> if v then 1 else 0)
        |> NMap.from_array)
  | Unary (o, e) ->
      let* n = n_eval_res e ctx in
      Ok (NMap.map (N_unary.eval o) n)
  | Binary (o, n1, n2) -> (
      let o = N_binary.eval o in
      let* n1 = n_eval_res n1 ctx in
      let* n2 = n_eval_res n2 ctx in
      try Ok (NMap.pointwise o n1 n2)
      with Division_by_zero -> Error ("division by zero: " ^ Exp.n_to_string n))
  | NIf (b, n1, n2) ->
      let* b = b_eval_res b ctx in
      let* n1 = n_eval_res n1 ctx in
      let* n2 = n_eval_res n2 ctx in
      Ok (n_map3 (fun b x1 x2 -> if b then x1 else x2) b n1 n2)
  | NCall (x, _) -> Error ("unknown function call: " ^ x)
  | ReadResult r -> Error ("unknown read: " ^ Variable.name r.array)

and b_eval_res (b : Exp.bexp) (ctx : t) : (BMap.t, string) Result.t =
  match b with
  | Bool b -> Ok (BMap.constant ~count:ctx.config.threads_per_warp ~value:b)
  | CastBool (CastInt e) -> b_eval_res e ctx
  | CastBool e ->
      let* e = n_eval_res e ctx in
      Ok (e |> NMap.to_array |> Array.map (fun v -> v <> 0) |> BMap.from_array)
  | NRel (o, n1, n2) ->
      let o = N_rel.eval o in
      let* n1 = n_eval_res n1 ctx in
      let* n2 = n_eval_res n2 ctx in
      Ok (n_map2 o n1 n2)
  | BRel (o, b1, b2) ->
      let o = B_rel.eval o in
      let* b1 = b_eval_res b1 ctx in
      let* b2 = b_eval_res b2 ctx in
      Ok (BMap.pointwise o b1 b2)
  | BNot b ->
      let* b = b_eval_res b ctx in
      Ok (BMap.map (fun x -> not x) b)
  | Pred (x, _) -> Error ("cannot evaluate predicate: " ^ x)
  | Distinct _ -> Error "cannot evaluate distinct"
  | AtomicResult _ -> Error "cannot evaluate atomic_result"
  | IsThreadUnif _ -> Error "cannot evaluate thread_unif"

let n_eval (e : Exp.nexp) (ctx : t) : NMap.t = n_eval_res e ctx |> Result.get_ok
let b_eval (e : Exp.bexp) (ctx : t) : BMap.t = b_eval_res e ctx |> Result.get_ok

let max_cost (m : Metric.t) (ctx : t) : Cost.t =
  let thread_count =
    match b_eval_res ctx.cond ctx with
    | Ok enabled -> BMap.count true enabled
    | Error _ -> ctx.config.threads_per_warp
  in
  Cost.from_int
    ~value:(Metric.max_cost thread_count ctx.config m)
    ~exact:false ()

let tid_count (ctx : t) : int =
  match b_eval_res ctx.cond ctx with
  | Ok enabled -> BMap.count true enabled
  | Error _ -> ctx.config.threads_per_warp

let to_cost ?(verbose = false) (m : Metric.t) (a : Access.t) (ctx : t) :
    (Cost.t, string) Result.t =
  let* idx =
    match a.index with
    | [ n ] -> Ok n
    | _ ->
        Error
          (Printf.sprintf "Unsupported access with multi-dimensional access: %s"
             (Access.to_string a))
  in
  let* idx = n_eval_res idx ctx in
  let* enabled = b_eval_res ctx.cond ctx in
  let tids = tid ctx in
  Metric.run ~verbose ctx.config m idx enabled tids

let bank_conflicts (index : Exp.nexp) (ctx : t) : (Cost.t, string) Result.t =
  let* idx = n_eval_res index ctx in
  let* enabled = b_eval_res ctx.cond ctx in
  let tids = tid ctx in
  Ok (Metric.BankConflicts.run ctx.config idx enabled tids)

let uncoalesced (index : Exp.nexp) (ctx : t) : (Cost.t, string) Result.t =
  let* idx = n_eval_res index ctx in
  let* enabled = b_eval_res ctx.cond ctx in
  let tids = tid ctx in
  Ok (Metric.UncoalescedAccesses.run ctx.config idx enabled tids)

let add = NMap.pointwise ( + )

type loop = Next of Range.t * t | End

let iter_res (r : Range.t) (ctx : t) : (loop, string) Result.t =
  let has_next = Range.has_next r in
  let* b = b_eval_res has_next ctx in
  if BMap.some_true b then
    let* lo = n_eval_res r.lower_bound ctx in
    let r = Range.next r in
    (* run one iteration *)
    Ok (Next (r, ctx |> restrict has_next |> put r.var lo))
  else Ok End

let iter (r : Range.t) (ctx : t) : loop = iter_res r ctx |> Result.get_ok

let is_active (ctx : t) : bool =
  (let* b = b_eval_res ctx.cond ctx in
   Ok (BMap.some_true b))
  |> Result.value ~default:false

let eval ?(verbose = false) (m : Metric.t)
    (linearize : Access.t -> (Access.t, string) Result.t) :
    Protocols.Code.t -> t -> int =
  let rec eval (cost : int) (p : Protocols.Code.t) (ctx : t) : int =
    let try_eval cost p ctx = if is_active ctx then eval cost p ctx else cost in
    match p with
    | Sync _ | Skip -> cost
    | Access a ->
        let c =
          match
            let* a = linearize a in
            let* c = to_cost ~verbose m a ctx in
            Ok c
          with
          | Ok c -> Cost.value c
          | Error e ->
              failwith
                ("Error: " ^ e ^ "\n - Access: " ^ Access.to_string a
               ^ "\nContext: " ^ to_string ctx)
        in
        cost + c
    | Decl { body = p; _ } -> eval cost p ctx
    | If (b, p, q) ->
        let cost = restrict b ctx |> try_eval cost p in
        restrict (Exp.b_not b) ctx |> try_eval cost q
    | Loop { cond_range; body } -> (
        match iter cond_range.range ctx with
        | Next (r, ctx') ->
            let cost = eval cost body ctx' in
            (* run the rest of the loop *)
            eval cost
              (Loop { cond_range = Cond_range.make r cond_range.cond; body })
              ctx
        | End ->
            (* Loop is done *)
            cost)
    | Seq (p, q) ->
        let cost = eval cost p ctx in
        eval cost q ctx
  in
  fun p ctx -> eval 0 p ctx
