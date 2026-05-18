%{
open Protocols
open Protocols.Exp
%}

%token <string> IDENT
%token <int> INT
%token <bool> BOOL

%token LPAREN RPAREN COMMA
%token PLUS MINUS MULT DIV MOD
%token PLUS_U MINUS_U MULT_U DIV_U MOD_U
%token BIT_AND BIT_OR BIT_XOR LEFT_SHIFT RIGHT_SHIFT URIGHT_SHIFT BIT_NOT
%token EQ NEQ LT LE GT GE
%token LT_U LE_U GT_U GE_U
%token L_AND L_OR L_NOT
%token QUESTION COLON
%token CAST_INT CAST_BOOL
%token EOF

(* C operator precedence (lowest to highest) *)
%right QUESTION COLON        (* ternary conditional *)
%left L_OR                   (* logical OR *)
%left L_AND                  (* logical AND *)
%left BIT_OR                 (* bitwise OR *)
%left BIT_XOR                (* bitwise XOR *)
%left BIT_AND                (* bitwise AND *)
%left EQ NEQ                 (* equality *)
%left LT LE GT GE LT_U LE_U GT_U GE_U  (* relational (signed + unsigned) *)
%left LEFT_SHIFT RIGHT_SHIFT URIGHT_SHIFT  (* bitwise shifts *)
%left PLUS MINUS PLUS_U MINUS_U  (* additive (signed + unsigned) *)
%left MULT DIV MOD MULT_U DIV_U MOD_U  (* multiplicative (signed + unsigned) *)
%right L_NOT BIT_NOT UMINUS  (* unary operators *)

%start <nexp> nexp_main
%start <bexp> bexp_main

%%

nexp_main:
  | n=nexp EOF { n }

bexp_main:
  | b=bexp EOF { b }

var:
  | id=IDENT { Variable.from_name id }

nexp:
  (* Literals *)
  | i=INT                                { Num i }
  | v=var                                { Var v }

  (* Function calls *)
  | name=IDENT LPAREN arg=nexp RPAREN    { NCall (name, [ arg ]) }

  (* Arithmetic binary operators (signed + unsigned variants) *)
  | left=nexp PLUS right=nexp            { Binary (N_binary.Plus Signedness.Signed, left, right) }
  | left=nexp PLUS_U right=nexp          { Binary (N_binary.Plus Signedness.Unsigned, left, right) }
  | left=nexp MINUS right=nexp           { Binary (N_binary.Minus Signedness.Signed, left, right) }
  | left=nexp MINUS_U right=nexp         { Binary (N_binary.Minus Signedness.Unsigned, left, right) }
  | left=nexp MULT right=nexp            { Binary (N_binary.Mult Signedness.Signed, left, right) }
  | left=nexp MULT_U right=nexp          { Binary (N_binary.Mult Signedness.Unsigned, left, right) }
  | left=nexp DIV right=nexp             { Binary (N_binary.Div Signedness.Signed, left, right) }
  | left=nexp DIV_U right=nexp           { Binary (N_binary.Div Signedness.Unsigned, left, right) }
  | left=nexp MOD right=nexp             { Binary (N_binary.Mod Signedness.Signed, left, right) }
  | left=nexp MOD_U right=nexp           { Binary (N_binary.Mod Signedness.Unsigned, left, right) }

  (* Bitwise binary operators *)
  | left=nexp BIT_AND right=nexp         { Binary (N_binary.BitAnd, left, right) }
  | left=nexp BIT_OR right=nexp          { Binary (N_binary.BitOr, left, right) }
  | left=nexp BIT_XOR right=nexp         { Binary (N_binary.BitXOr, left, right) }
  | left=nexp LEFT_SHIFT right=nexp      { Binary (N_binary.LeftShift, left, right) }
  | left=nexp RIGHT_SHIFT right=nexp     { Binary (N_binary.RightShift Signedness.Signed, left, right) }
  | left=nexp URIGHT_SHIFT right=nexp    { Binary (N_binary.RightShift Signedness.Unsigned, left, right) }

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

  (* Numeric comparisons (signed + unsigned variants). [Eq] / [Neq]
     are bit-equality / inequality and have no signedness. *)
  | left=nexp EQ right=nexp              { NRel (N_rel.Eq, left, right) }
  | left=nexp NEQ right=nexp             { NRel (N_rel.Neq, left, right) }
  | left=nexp LT right=nexp              { NRel (N_rel.Lt Signedness.Signed, left, right) }
  | left=nexp LT_U right=nexp            { NRel (N_rel.Lt Signedness.Unsigned, left, right) }
  | left=nexp LE right=nexp              { NRel (N_rel.Le Signedness.Signed, left, right) }
  | left=nexp LE_U right=nexp            { NRel (N_rel.Le Signedness.Unsigned, left, right) }
  | left=nexp GT right=nexp              { NRel (N_rel.Gt Signedness.Signed, left, right) }
  | left=nexp GT_U right=nexp            { NRel (N_rel.Gt Signedness.Unsigned, left, right) }
  | left=nexp GE right=nexp              { NRel (N_rel.Ge Signedness.Signed, left, right) }
  | left=nexp GE_U right=nexp            { NRel (N_rel.Ge Signedness.Unsigned, left, right) }

  (* Predicate call with 2+ arguments — [name(arg1, arg2, ...)].
     The arity-2+ form disambiguates from [NCall (name, arg)] in
     [nexp] (which is exactly 1 argument and would otherwise create
     a shift/reduce conflict on [IDENT LPAREN nexp RPAREN]).
     Unary predicates ([pow2], [nonneg], [uintN]) have inline
     bodies in [Predicates.all_predicates]; their canonical
     round-trip form runs through [Predicates.b_inline] before
     printing, so [nonneg(v)] becomes [v >= 0] and [pow2(v)]
     becomes the disjunction of [v == 2^k] equalities. *)
  | name=IDENT LPAREN first=nexp COMMA
        rest=separated_nonempty_list(COMMA, nexp) RPAREN
                                         { Pred (name, first :: rest) }

  (* Boolean binary operators *)
  | left=bexp L_AND right=bexp           { BRel (B_rel.BAnd, left, right) }
  | left=bexp L_OR right=bexp            { BRel (B_rel.BOr, left, right) }

  (* Boolean unary operator *)
  | L_NOT expr=bexp                      { BNot expr }

  (* Type cast *)
  | CAST_BOOL LPAREN n=nexp RPAREN       { CastBool n }

  (* Parentheses *)
  | LPAREN b=bexp RPAREN                 { b }
