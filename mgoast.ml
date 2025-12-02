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

(* TODO: fonction pour afficher la location *)

type location = Lexing.position * Lexing.position (*debut et fin de l'ident*)
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
  | Set    of (expr list) * (expr list) (*a =b*)
  | Inc    of expr
  | Dec    of expr
  (* Structures de contrôle usuelles *)
  | If     of expr * seq * seq
  | For    of expr * seq
  | Block  of seq  (* On l'oublie *)
  (* Déclaration de variable locales *)
  | Vars   of ident list * typ option * expr list  (*Avant, on avait seq list (plutot que expr list)*)
  (*Déclaration et affectation d'une variable locale sans type explicite*)
  | Pset of ident list * expr list
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


type expr_typed = { edesc_t : expr_desc_t; etype: typ option}
and expr_desc_t = 
  (* Base arithmétique et logique *)
  | Int_t    of int64
  | Bool_t   of bool
  | String_t of string
  | Unop_t   of unop * expr_typed
  | Binop_t  of binop * expr_typed * expr_typed
  (* Accès à une variable ou un champs *)
  | Var_t    of ident
  | Dot_t    of expr_typed * ident
  (* Pointeur nul *)
  | Nil_t
  (* Création d'une nouvel structure *)
  | New_t  of string
  (* Appel de fonction *)
  | Call_t of ident * expr_typed list
  (* Fonction primitive pour impression *)
  | Print_t  of expr_typed list

type instr_t = 
  (* Écriture dans une variable ou un attribut *)
  | Set_t    of (expr_typed list) * (expr_typed list)
  | Inc_t    of expr_typed
  | Dec_t    of expr_typed
  (* Structures de contrôle usuelles *)
  | If_t     of expr_typed * seq_t * seq_t
  | For_t    of expr_typed * seq_t
  | Block_t  of seq_t  (* On l'oublie *)
  (* Déclaration de variable locales *)
  | Vars_t   of ident list * typ option * expr_typed list  (*Avant, on avait seq list (plutot que expr_typed list)*)
  (*Déclaration et affectation d'une variable locale sans type explicite*)
  | Pset_t of ident list * expr_typed list
  (* Fin d'une fonction *)
  | Return_t of expr_typed list
  (* expr_typedession utilisée comme instruction *)
  | Expr_t   of expr_typed

and seq_t = instr_t list


type func_def_typed = {
    fname_t: ident;
    params_t: (ident * typ) list;
    return_t: typ list;
    body_t: seq_t;
  }

type decl_typed =
  | Fun_t of func_def_typed
  | Struct_t  of struct_def

(* Programme complet : indication de l'import de fmt + liste de déclarations *)

type program_typed = decl_typed list