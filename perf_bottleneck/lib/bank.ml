open Stage0
module IntMap = Common.IntMap
module Variable = Protocols.Variable
open Protocols
open Rel_cost

module Code = struct
  type t =
    | Index of Exp.nexp
    | Loop of { range : Range.t; body : t }
    | Cond of Exp.bexp * t
    | Decl of { var : Variable.t; ty : C_type.t; body : t }

  module SubstMake (S : Subst.SUBST) = struct
    module M = Subst.Make (S)

    let rec subst (s : S.t) : t -> t = function
      | Loop { range = r; body = acc } ->
          Loop { range = M.r_subst s r; body = subst s acc }
      | Cond (b, acc) -> Cond (M.b_subst s b, subst s acc)
      | Index a -> Index (M.n_subst s a)
      | Decl { var; ty; body } as i ->
          M.add s var (function
            | Some s -> Decl { var; ty; body = subst s body }
            | None -> i)
  end

  module S1 = SubstMake (Subst.SubstPair)

  let subst = S1.subst

  let normalize : t -> t =
    let rec norm : t -> t = function
      | Index _ as s -> s
      | Cond (e, s) -> Cond (e, norm s)
      | Decl { var; ty; body } -> Decl { var; ty; body = norm body }
      | Loop { range; body } ->
          (* x' := x + lb *)
          let new_x =
            Range.Step.to_inc range.step (Var range.var) range.lower_bound
          in
          Loop
            {
              range = Range.to_zero range;
              body = norm (subst (range.var, new_x) body);
            }
    in
    norm

  module Erase_context = struct
    type prog = t

    type t = {
      globals : Variable.Set.t;
      result : prog;
      locals : Variable.Set.t;
    }

    (* Erase some of the context to simplify analysis *)
    let rec minimal (locals : Variable.Set.t) : prog -> t = function
      | Index e as s ->
          let fns = Exp.n_free_names e Variable.Set.empty in
          {
            globals = Variable.Set.diff fns locals;
            result = s;
            locals = Variable.Set.inter fns locals;
          }
      | Decl { var; ty; body } ->
          let { globals; result = body; locals } =
            minimal (Variable.Set.add var locals) body
          in
          let result =
            if Variable.Set.mem var locals then
              (* conver decl into cond *)
              Cond (Range.decl_to_bexp var ty, body)
            else
              (* discard decl altogether *)
              body
          in
          { locals; globals; result }
      | Loop { range = { var; ty; _ } as r; body } ->
          (* Convert loops into declarations *)
          let new_locals =
            if Range.intersects locals r then Variable.Set.add var locals
            else locals
          in
          let { globals; locals; result = body } = minimal new_locals body in
          let result =
            if Variable.Set.mem var locals || Variable.Set.mem var globals then
              (* convert to cond *)
              Cond (Range.decl_to_bexp var ty, body)
            else
              (* loop does not affect index *)
              body
          in
          { globals; locals; result }
      | Cond (BRel (BAnd, e1, e2), s) ->
          minimal locals (Cond (e1, Cond (e2, s)))
      | Cond (e, s) when Exp.b_intersects locals e ->
          let { globals; locals; result = s } = minimal locals s in
          { globals; locals; result = Cond (e, s) }
      | Cond (_, s) -> minimal locals s
  end

  let erase_context ~locals : t -> Erase_context.t =
   fun s -> Erase_context.minimal locals s

  let to_ra (idx_analysis : Variable.Set.t -> Exp.nexp -> int) :
      Variable.Set.t -> t -> Ra.Stmt.t =
    let rec to_ra (locals : Variable.Set.t) : t -> Ra.Stmt.t = function
      | Index a -> Tick (idx_analysis locals a)
      | Cond (_, p) -> to_ra locals p
      | Decl { var = x; body = p; _ } -> to_ra (Variable.Set.add x locals) p
      | Loop { range; body } -> Loop { range; body = to_ra locals body }
    in
    to_ra

  let rec to_string ?(array = "") : t -> string = function
    | Loop { range = r; body = acc } ->
        "for (" ^ Range.to_string r ^ ")\n" ^ to_string acc
    | Cond (b, acc) -> "if (" ^ Exp.b_to_string b ^ ")\n" ^ to_string acc
    | Index a -> array ^ "[" ^ Exp.n_to_string a ^ "]"
    | Decl { var = x; body = p; _ } ->
        "var " ^ Variable.name x ^ " " ^ to_string p ^ "\n"

  (* Returns the set of local variables mentioned in the index *)
  let locals ~init : t -> Variable.Set.t =
    let rec locals (fns : Variable.Set.t) : t -> Variable.Set.t = function
      | Index _ -> fns
      | Loop { range = r; body = p } ->
          let r_fns = Range.free_names r Variable.Set.empty in
          let fns =
            (* Test if the loop range contains thread-locals *)
            if Variable.Set.inter r_fns fns |> Variable.Set.is_empty then fns
            else Variable.Set.add r.var fns
          in
          locals fns p
      | Cond (_, p) -> locals fns p
      | Decl { var; ty = _; body } -> locals (Variable.Set.add var fns) body
    in
    locals init

  let rec map_index (f : Exp.nexp -> Exp.nexp) : t -> t = function
    | Index a -> Index (f a)
    | Loop { range = r; body = p } -> Loop { range = r; body = map_index f p }
    | Cond (e, p) -> Cond (e, map_index f p)
    | Decl { var; ty; body } -> Decl { var; ty; body = map_index f body }

  let rec to_bexp : t -> Exp.bexp = function
    | Index _ -> Exp.b_true
    | Cond (b, p) -> Exp.b_and b (to_bexp p)
    | Decl { var; ty; body } ->
        Exp.b_and (Range.decl_to_bexp var ty) (to_bexp body)
    | Loop { range; body } -> Exp.b_and (Range.to_cond range) (to_bexp body)

  let cond_size (e : t) : int =
    Exp.b_free_names (to_bexp e) Variable.Set.empty |> Variable.Set.cardinal

  let rec index : t -> Exp.nexp = function
    | Index a -> a
    | Loop { body = p; _ } | Cond (_, p) | Decl { body = p; _ } -> index p

  let index_size (e : t) : int =
    Exp.n_free_names (index e) Variable.Set.empty |> Variable.Set.cardinal

  let flatten (a : t) : t = Index (index a)

  let trim_decls : t -> t =
    let rec opt : t -> Variable.Set.t * t = function
      | Index e -> (Exp.n_free_names e Variable.Set.empty, Index e)
      | Loop { range = r; body = a } ->
          let fns, a = opt a in
          (Range.free_names r fns, Loop { range = r; body = a })
      | Cond (e, a) ->
          let fns, a = opt a in
          (Exp.b_free_names e fns, Cond (e, a))
      | Decl { var; ty; body = a } ->
          let fns, a = opt a in
          let a =
            if Variable.Set.mem var fns then Decl { var; ty; body = a } else a
          in
          (fns, a)
    in
    fun a -> opt a |> snd

  let to_approx (x : Variable.t) : t -> Approx.Code.t =
    let rec to_approx : t -> Approx.Code.t = function
      | Index a -> Access (Access.read x [ a ])
      | Loop { range = r; body = a } -> Loop { range = r; body = to_approx a }
      | Cond (b, a) -> Cond (b, to_approx a)
      | Decl { var; ty = _; body } -> Approx.Code.decl var (to_approx body)
    in
    to_approx

  let gen_random (_ : Variable.t) (ctx : Vectorized.t) :
      Vectorized.NMap.t Option.t =
    Some (Vectorized.NMap.random ctx.thread_count ())

  let eval_res ?(max_cost = -1) (cfg : Config.t) (m : Metric.t) :
      Vectorized.t -> t -> (Cost.t, string) Result.t =
    let ( let* ) = Result.bind in
    fun ctx ->
      let max_cost : Cost.t =
        if max_cost < 0 then
          Metric.max_cost_from cfg m |> fun value ->
          Cost.from_int ~value ~exact:true ()
        else Cost.from_int ~value:max_cost ~exact:true ()
      in
      let rec eval (c : Cost.t) (ctx : Vectorized.t) :
          t -> (Cost.t, string) Result.t = function
        | Index a -> Vectorized.to_cost m a ctx
        | Decl { body = a; _ } ->
            (* Ignore variables so that if eval uses an unknown variable it
           gets stuck. *)
            eval c ctx a
        | Cond (e, a) ->
            let* v = Vectorized.b_eval_res e ctx in
            if Vectorized.BMap.some_true v then
              eval c (Vectorized.restrict e ctx) a
            else Ok c
        | Loop { range = r; body = a } -> (
            let* l = Vectorized.iter_res r ctx in
            match l with
            | Next (r, ctx') ->
                let* c = eval c ctx' a in
                if Cost.(c >= max_cost) then Ok c
                else eval c ctx (Loop { range = r; body = a })
            | End -> Ok c)
      in
      eval Cost.zero ctx

  module Make (L : Logger.Logger) = struct
    module M = Metric_analysis.Make (L)
    module L = Linearize_index.Make (L)

    let index_cost ~local_variables (config : Config.t) (m : Metric.t) (a : t) :
        Metric_analysis.IndexCost.t =
      let index = index a in
      let locals = locals ~init:local_variables a in
      let divergence = to_bexp a in
      M.run m config ~strategy:Analysis_strategy.OverApproximation ~locals
        ~index ~divergence

    let from_proto (arrays : Memory.t Variable.Map.t) (cfg : Config.t) :
        Variable.Set.t -> Protocols.Code.t -> (Variable.t * t) Seq.t =
      let lin = L.linearize cfg arrays in
      let rec on_p (locals : Variable.Set.t) : Code.t -> (Variable.t * t) Seq.t
          = function
        | Access { array = x; index = l; _ } ->
            l |> lin x
            |> Option.map (fun e -> Seq.return (x, Index e))
            |> Option.value ~default:Seq.empty
        | Sync _ -> Seq.empty
        | Decl { body = p; var; ty } ->
            p
            |> on_p (Variable.Set.add var locals)
            |> Seq.map (fun (x, i) -> (x, Decl { var; body = i; ty }))
        | If (b, p, q) ->
            Seq.append
              (on_p locals p |> Seq.map (fun (x, p) -> (x, Cond (b, p))))
              (on_p locals q
              |> Seq.map (fun (x, q) -> (x, Cond (Exp.b_not b, q))))
        | Loop { range = r; body = p } ->
            let locals =
              let r_locals = Range.free_names r Variable.Set.empty in
              if Variable.Set.inter locals r_locals |> Variable.Set.is_empty
              then locals
              else Variable.Set.add (Range.var r) locals
            in
            on_p locals p
            |> Seq.map (fun (x, i) -> (x, Loop { range = r; body = i }))
        | Skip -> Seq.empty
        | Seq (p, q) -> Seq.append (on_p locals p) (on_p locals q)
      in
      on_p
  end

  module Silent = Make (Logger.Silent)
  module Default = Make (Logger.Colors)

  let index_cost = Default.index_cost

  let from_proto :
      Memory.t Variable.Map.t ->
      Config.t ->
      Variable.Set.t ->
      Code.t ->
      (Variable.t * t) Seq.t =
    Default.from_proto
end

type t = {
  (* The kernel name *)
  name : string;
  (* The array name *)
  array : Variable.t;
  (* Hierarchy *)
  hierarchy : Mem_hierarchy.t;
  (* The internal variables are used in the code of the kernel.  *)
  global_variables : Variable.Set.t;
  (* The internal variables are used in the code of the kernel.  *)
  local_variables : Variable.Set.t;
  (* The code of a kernel performs the actual memory accesses. *)
  code : Code.t;
}

let location (k : t) : Location.t = Variable.location k.array

let erase_context (k : t) : t =
  let open Code.Erase_context in
  let { locals; globals; result } =
    Code.erase_context ~locals:k.local_variables k.code
  in
  { k with local_variables = locals; global_variables = globals; code = result }

let to_string (k : t) : string =
  Code.to_string ~array:(Variable.name k.array) k.code

let map_index (f : Exp.nexp -> Exp.nexp) (k : t) : t =
  { k with code = Code.map_index f k.code }

let to_check (k : t) : Approx.Check.t =
  let code = Code.to_approx k.array k.code in
  let vars = Variable.Set.union k.global_variables Variable.tid_set in
  Approx.Check.from_code vars code

let index_size (k : t) : int = k.code |> Code.index_size
let cond_size (k : t) : int = k.code |> Code.cond_size
let normalize (k : t) : t = { k with code = Code.normalize k.code }

let index_cost (params : Config.t) (m : Metric.t) (k : t) :
    Metric_analysis.IndexCost.t =
  Code.index_cost ~local_variables:k.local_variables params m k.code

let trim_decls (k : t) : t = { k with code = Code.trim_decls k.code }

module Make (L : Logger.Logger) = struct
  module L = Linearize_index.Make (L)

  (*
  Given a kernel return a sequence of slices.
  *)
  let from_proto (cfg : Config.t) (k : Kernel.t) : t Seq.t =
    let local_variables = Params.to_set k.local_variables in
    k.code
    |> Protocols.Code.subst_block_dim cfg.block_dim
    |> Protocols.Code.subst_grid_dim cfg.grid_dim
    |> Code.from_proto k.arrays cfg local_variables
    |> Seq.map (fun (array, p) ->
           let code = if k.pre = Bool true then p else Code.Cond (k.pre, p) in
           {
             name = k.name;
             hierarchy = Variable.Map.find array k.arrays |> Memory.hierarchy;
             global_variables = Params.to_set k.global_variables;
             local_variables;
             code;
             array;
           })
end

let eval_res ?(max_cost = -1) (params : Config.t) (m : Metric.t) (k : t) :
    (Cost.t, string) Result.t =
  let ctx = Vectorized.from_config params in
  Code.eval_res ~max_cost params m ctx k.code

module Silent = Make (Logger.Silent)
module Default = Make (Logger.Colors)

let to_ra (idx_analysis : Variable.Set.t -> Exp.nexp -> int) : t -> Ra.Stmt.t =
 fun k -> Code.to_ra idx_analysis k.local_variables k.code

let from_proto : Config.t -> Kernel.t -> t Seq.t = Default.from_proto
let flatten (k : t) : t = { k with code = Code.flatten k.code }
