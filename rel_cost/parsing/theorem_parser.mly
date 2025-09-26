%{
open Protocols
open Protocols.Exp
open Theorem_file
open Rel_cost
%}

%token <string> IDENT
%token <int> INT
%token <bool> BOOL

%token LPAREN RPAREN
%token PLUS MINUS MULT DIV MOD
%token BIT_AND BIT_OR BIT_XOR LEFT_SHIFT RIGHT_SHIFT BIT_NOT
%token EQ NEQ LT LE GT GE
%token L_AND L_OR L_NOT
%token QUESTION COLON
%token CAST_INT CAST_BOOL LOCALS X Y Z LBRACKET RBRACKET COMMA LOCAL_CONTEXT
%token GLOBAL_CONTEXT LBRACE THREADS_PER_WARP SEMICOLON RBRACE
%token BLOCK_DIM PROVE MAX MIN
%token EOF

(* C operator precedence (lowest to highest) *)
%right QUESTION COLON        (* ternary conditional *)
%left L_OR                   (* logical OR *)
%left L_AND                  (* logical AND *)
%left BIT_OR                 (* bitwise OR *)
%left BIT_XOR                (* bitwise XOR *)
%left BIT_AND                (* bitwise AND *)
%left EQ NEQ                 (* equality *)
%left LT LE GT GE            (* relational *)
%left LEFT_SHIFT RIGHT_SHIFT (* bitwise shifts *)
%left PLUS MINUS             (* additive *)
%left MULT DIV MOD           (* multiplicative *)
%right L_NOT BIT_NOT UMINUS  (* unary operators *)

%start <Theorem_file.t> file_main
%start <nexp> nexp_main
%start <bexp> bexp_main

%%

nexp_main:
  | n=nexp EOF { n }

bexp_main:
  | b=bexp EOF { b }

var:
  | id=IDENT { Variable.from_name id }
  | THREADS_PER_WARP { Variable.from_name "threads_per_warp" }
  | BLOCK_DIM { Variable.from_name "block_dim" }
  | LOCALS { Variable.from_name "locals" }
  | LOCAL_CONTEXT { Variable.from_name "local_context" }
  | GLOBAL_CONTEXT { Variable.from_name "global_context" }
  | X { Variable.from_name "x" }
  | Y { Variable.from_name "y" }
  | Z { Variable.from_name "z" }

nexp:
  (* Literals *)
  | i=INT                                { Num i }
  | v=var                                { Var v }

  (* Function calls *)
  | name=IDENT LPAREN arg=nexp RPAREN    { NCall (name, arg) }

  (* Arithmetic binary operators *)
  | left=nexp PLUS right=nexp            { Binary (N_binary.Plus, left, right) }
  | left=nexp MINUS right=nexp           { Binary (N_binary.Minus, left, right) }
  | left=nexp MULT right=nexp            { Binary (N_binary.Mult, left, right) }
  | left=nexp DIV right=nexp             { Binary (N_binary.Div, left, right) }
  | left=nexp MOD right=nexp             { Binary (N_binary.Mod, left, right) }

  (* Bitwise binary operators *)
  | left=nexp BIT_AND right=nexp         { Binary (N_binary.BitAnd, left, right) }
  | left=nexp BIT_OR right=nexp          { Binary (N_binary.BitOr, left, right) }
  | left=nexp BIT_XOR right=nexp         { Binary (N_binary.BitXOr, left, right) }
  | left=nexp LEFT_SHIFT right=nexp      { Binary (N_binary.LeftShift, left, right) }
  | left=nexp RIGHT_SHIFT right=nexp     { Binary (N_binary.RightShift, left, right) }

  (* Unary operators *)
  | MINUS expr=nexp %prec UMINUS         { Unary (N_unary.Negate, expr) }
  | BIT_NOT expr=nexp                    { Unary (N_unary.BitNot, expr) }

  (* Conditional expression *)
  | cond=bexp QUESTION then_expr=nexp COLON else_expr=nexp { NIf (cond, then_expr, else_expr) }

  (* Type cast *)
  | CAST_INT LPAREN b=bexp RPAREN        { CastInt b }

  (* Parentheses *)
  | LPAREN n=nexp RPAREN                 { n }

bexp:
  (* Literals *)
  | b=BOOL                               { Bool b }
  
  (* Numeric comparisons *)
  | left=nexp EQ right=nexp              { NRel (N_rel.Eq, left, right) }
  | left=nexp NEQ right=nexp             { NRel (N_rel.Neq, left, right) }
  | left=nexp LT right=nexp              { NRel (N_rel.Lt, left, right) }
  | left=nexp LE right=nexp              { NRel (N_rel.Le, left, right) }
  | left=nexp GT right=nexp              { NRel (N_rel.Gt, left, right) }
  | left=nexp GE right=nexp              { NRel (N_rel.Ge, left, right) }
  
  (* Boolean binary operators *)
  | left=bexp L_AND right=bexp           { BRel (B_rel.BAnd, left, right) }
  | left=bexp L_OR right=bexp            { BRel (B_rel.BOr, left, right) }
  
  (* Boolean unary operator *)
  | L_NOT expr=bexp                      { BNot expr }
  
  (* Type cast *)
  | CAST_BOOL LPAREN n=nexp RPAREN       { CastBool n }
  
  (* Parentheses *)
  | LPAREN b=bexp RPAREN                 { b }

file_main:
  | f=file EOF { f }

%inline a_field: field SEMICOLON { $1 }

file:
  | fields=a_field* goals=theorem_statement* {
      let file = List.fold_left (fun acc setter -> setter acc) make fields in
      List.fold_left (fun acc goal -> add_goal goal acc) file goals
    }

field:
  | THREADS_PER_WARP COLON value=INT { set_threads_per_warp value }
  | BLOCK_DIM COLON dim=dim3_object { set_block_dim dim }
  | LOCALS COLON vars=variable_list { set_locals vars }
  | LOCAL_CONTEXT COLON expr=bexp { set_local_context expr }
  | GLOBAL_CONTEXT COLON expr=bexp { set_global_context expr }

theorem_statement:
  | PROVE prop=bexp { Symbolic_metric_analysis.Theorem.Goal.Prop prop }
  | MAX expr=nexp   { Symbolic_metric_analysis.Theorem.Goal.Optimize {strategy = Protocols.Gen_z3.Optimizer.Strategy.Maximize; expr} }
  | MIN expr=nexp   { Symbolic_metric_analysis.Theorem.Goal.Optimize {strategy = Protocols.Gen_z3.Optimizer.Strategy.Minimize; expr} }

dim3_object:
  | LBRACE l=separated_list(COMMA, dim3_field) RBRACE {
      List.fold_left (fun acc setter -> setter acc) Dim3.one l
    }

dim3_field:
  | separated_pair(X, COLON, INT) { Dim3.set_x (snd $1) }
  | separated_pair(Y, COLON, INT) { Dim3.set_y (snd $1) }
  | separated_pair(Z, COLON, INT) { Dim3.set_z (snd $1) }

variable_list:
  | l=delimited(LBRACKET, separated_list(COMMA, var), RBRACKET) {
      l
    }
