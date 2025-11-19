%{

  open Lexing
  open Mgoast

  exception Error

%}

%token <int64> INT
%token <string> IDENT
%token <string> STRING
%token PACKAGE IMPORT TYPE STRUCT ELSE FALSE FOR FUNC IF NIL RETURN TRUE VAR
%token LPAR RPAR BEGIN END SEMI STAR OR AND EQ NEQ GT GE LT LE ADD SUB SUBU MUL DIV REM DOT NOT
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
    { if main="main" then (false, decls) else raise Error}
| PACKAGE main=IDENT SEMI IMPORT fmt=STRING SEMI decls=list(decl) EOF
    { if main="main" && fmt="fmt" then (true, decls) else raise Error} 
;

ident:
  id = IDENT 
    { { loc = $startpos, $endpos; id = id } }  //loc = (int*int) id =id
;

decl:
  | struct
  | func
;

struct:
  | TYPE id=ident STRUCT BEGIN fl=loption(fields) END SEMI
      { Struct { sname = id; fields = List.flatten fl; } }
;

func:
  | FUNC id=ident LPAR fl=loption(fields) RPAR // ATTENTION CEST PAS FINI

mgotype:
  | STAR s=IDENT { TStruct(s) }
;

vars:
  | id = ident

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
;
