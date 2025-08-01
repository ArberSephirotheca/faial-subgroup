%{
open Protocols.Gen_z3
open Tactic
%}

%token <string> IDENT
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> STRING

%token LPAREN RPAREN LBRACE RBRACE
%token SEMI L_OR L_AND NOT COMMA COLON
%token IF ELSE FAIL_IF_NOT_DECIDED TIMEOUT REPEAT
%token WITH SKIP FAIL PAR SEQ SEC MILLI_SEC
%token EOF


%left L_OR
%left L_AND
%right NOT
%nonassoc IF
%nonassoc ELSE

%start <Tactic.t> main
%type <Probe.t> probe
%%

main:
  | tactic_expr EOF { $1 }

(* Top level *)
tactic_expr:
  | seq=list(statement) { and_then_ex seq }

(* Any statement - can be compound or simple *)
statement:
  | stmt=compound_stmt { stmt }
  | instr=tactic_instr SEMI { instr }
  | SEMI { Skip }


time:
  | t=INT SEC { t * 1000 }
  | t=INT MILLI_SEC { t }

(* Compound statements (no dangling else issues) *)
compound_stmt:
  | IF LPAREN probe=probe RPAREN then_tactic=statement %prec IF
    { Cond { probe; then_tactic; else_tactic=Skip } }
  | IF LPAREN probe=probe RPAREN then_tactic=statement ELSE else_tactic=statement
    { Cond { probe; then_tactic; else_tactic } }
  | TIMEOUT timeout_ms=time body=statement
    { TryFor { timeout_ms; body } }
  | REPEAT max_iterations=INT body=statement
    { Repeat { max_iterations; body } }
  | WITH LPAREN params=param_list RPAREN body=statement
    { UsingParams { params; body } }
  | PAR LBRACE seq=list(statement) RBRACE
    { ParOr seq }
  | SEQ? LBRACE seq=list(statement) RBRACE
    { and_then_ex seq }

tactic_instr:
  | IDENT  { Tactic $1 }
  | FAIL_IF_NOT_DECIDED      { FailIfNotDecided }
  | SKIP                     { Skip }
  | FAIL                     { Fail }

probe:
  | IDENT                    { Probe $1 }
  | FLOAT                    { Const $1 }
  | probe L_AND probe          { And { left = $1; right = $3 } }
  | probe L_OR probe           { Or { left = $1; right = $3 } }
  | NOT e=probe                { Not e }
  | LPAREN probe RPAREN      { $2 }

param_list:
  | param COMMA param_list  { $1 :: $3 }
  | param                   { [$1] }
  | param COMMA             { [$1] }

param:
  | IDENT COLON BOOL               { ($1, Params.Value.Bool $3) }
  | IDENT COLON INT                { ($1, Params.Value.Int $3) }
  | IDENT COLON FLOAT              { ($1, Params.Value.Float $3) }
  | IDENT COLON STRING             { ($1, Params.Value.String $3) }


