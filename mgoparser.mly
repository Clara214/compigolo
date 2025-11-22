%{

  open Lexing
  open Mgoast

  exception Error

%}

%token <int64> INT
%token <string> IDENT
%token <string> STRING
%token <bool> BOOL
%token TBOOL TSTRING TINT
%token PACKAGE IMPORT TYPE STRUCT ELSE FALSE FOR FUNC IF NIL RETURN TRUE VAR FMT PRINT
%token LPAR RPAR BEGIN END SEMI STAR OR AND EQ NEQ GT GE LT LE ADD SUB SUBU MUL DIV REM DOT NOT COMA ADDADD SUBSUB SET PSET
%token EOF

%left OR
%left AND
%left EQ NEQ LT LE GT GE
%left ADD SUB
%left MUL DIV REM
%nonassoc SUBU NOT
%left DOT

%start prog
%type <Mgoast.program> prog

%%

prog:
| PACKAGE main=IDENT SEMI decls=list(decl) EOF //package main, fmt optionnel, suite de declaration, end of file
    { if main="main" then (false, decls) else raise Error }
| PACKAGE main=IDENT SEMI IMPORT fmt=STRING SEMI decls=list(decl) EOF
    { if main="main" && fmt="fmt" then (true, decls) else raise Error } 
;

ident:
  id = IDENT 
    { { loc = $startpos, $endpos; id = id } }  //loc = (int*int) id =id
;

decl:
 TYPE id=ident STRUCT BEGIN fl=loption(fields) END SEMI
  { Struct { sname = id; fields = List.flatten fl; } }
;

structure:
  | TYPE id=ident STRUCT BEGIN fl=loption(fields) END SEMI
      { Struct { sname = id; fields = List.flatten fl; } }
;

func:
  | FUNC id=ident LPAR fl=loption(fields) RPAR // ATTENTION CEST PAS FINI
  {}

mgotype:
  | STAR s=IDENT { TStruct(s) }
  | TINT { TInt }
  | TSTRING { TString }
  | TBOOL { TBool }
;

vars:
  | id = ident
  {}

varstyp:
  |  x=ident t=mgotype               {[(x,t)]}

fields:
| xt=varstyp SEMI?              { [xt]      }
| xt=varstyp SEMI xtl = fields  { xt :: xtl }

expr:
| e = expr_desc {  { eloc = $startpos, $endpos; edesc = e } }
;

expr_desc:
| n=INT { Int(n) }
| s=STRING { String(s) }
| TRUE { Bool(true) }
| FALSE { Bool(false) }
| NIL { Nil }
| LPAR e=expr RPAR { e }
| v=IDENT { Var(v) }
| e=expr DOT i=IDENT { Dot(e, i) }
| i=IDENT LPAR s = separated_list(COMA, expr ) RPAR  {Call(i,s)}
|FMT DOT PRINT LPAR s = separated_list(COMA, expr) RPAR {Print(s)}
//unop
| e = expr o=unop {o,e}
//binop 
| e1 = expr o=binop e2 = expr {Binop(o,e1,e2)}
;

unop:
|SUB{Opp}
|NOT{Not}

binop:
| ADD{Add}
| SUB{Sub}
| MUL{Mul}
| DIV{Div}
| REM{Rem}
| AND{And}
| OR{Or}
| EQ{Eq}
|NEQ{Neq}
|LT{Lt}
|LE{Le}
|GT{Gt}
|GE{Ge}

;
instr_simple:
|e = expr {e}
|e=expr ADDADD {Inc(e ADDADD)}
|e= expr SUBSUB {Dec(e SUBSUB)}
|e1 = separated_nonempty_list(COMA,expr) SET e2=separated_nonempty_list(COMA,expr) {Set(e1,e2)}
(*|i = separated_nonempty_list(COMA,IDENT) PSET e=separated_nonempty_list(COMA,expr) {}*)

instr_if:
|IF e=expr b=bloc {If(e, )} (*todo*)
bloc:
|BEGIN i1 = separated_list(SEMI,instr) option(instr) {block(i1)}  (*bon comportement ?*)




