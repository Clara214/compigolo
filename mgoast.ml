(**
   Petit Go : un petit langage impératif avec structures inspiré de Go
*)

(* Types déclarés pour les champs, pour les variables, et pour les 
   paramètres et résultats des méthodes. *)
type typ =
  | TInt
  | TBool
  | TString
  | TStruct of string

let typ_to_string = function
  | TInt     -> "int"
  | TBool    -> "bool"
  | TString  -> "string"
  | TStruct c -> ("*"^c)

type unop  = Opp | Not
type binop = Add | Sub | Mul | Div | Rem
           | Lt  | Le  | Gt | Ge | Eq  | Neq
           | And | Or

(* Pour la localisation des erreurs de typage les positions de début et de fin
   des expressions sont conservées dans l'ast
   ils sont construits à l'aide des références $startpos et $endpos
   dans les actions de la grammaire
*)

type location = Lexing.position * Lexing.position
type ident = { loc : location; id : string }

(* Expressions *)
type expr =  { edesc : expr_desc; eloc  : location; }
and expr_desc = 
  (* Base arithmétique et logique *)
  | Int    of int64
  | Bool   of bool
  | String of string
  | Unop   of unop * expr
  | Binop  of binop * expr * expr
  (* Accès à une variable ou un champs *)
  | Var    of ident
  | Dot    of expr * ident
  (* Pointeur nul *)
  | Nil
  (* Création d'une nouvel structure *)
  | New  of string
  (* Appel de fonction *)
  | Call of ident * expr list
  (* Fonction primitive pour impression *)
  | Print  of expr list
  
(* Instructions *)
type instr = { idesc : instr_desc; iloc  : location; }
and instr_desc = 
  (* Écriture dans une variable ou un attribut *)
  | Set    of (expr list) * (expr list)
  | Inc    of expr
  | Dec    of expr
  (* Structures de contrôle usuelles *)
  | If     of expr * seq * seq
  | For  of expr * seq
  | Block of seq
  (* Déclaration de variable locales *)
  | Vars of ident list * typ option * seq
  (* Fin d'une fonction *)
  | Return of expr list
  (* Expression utilisée comme instruction *)
  | Expr   of expr

and seq = instr list

(* Définition de fonction 

   Syntaxe :  <nom> (<params>) <type de retour> { ... }

   Le corps de la méthode est similaire au corps d'une fonction. *)
type func_def = {
    fname: ident;
    params: (ident * typ) list;
    return: typ list;
    body: seq;
  }
        
(* Définition de structures : nom et déclaration des champs *)
type struct_def = {
    sname: ident;
    fields: (ident * typ) list;
  }
  
type decl =
  | Fun of func_def
  | Struct  of struct_def

(* Programme complet : indication de l'import de fmt + liste de déclarations *)

type program = bool * decl list
