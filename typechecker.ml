open Mgoast

exception Error of Mgoast.location * string
let error loc s = raise (Error (loc,s))

let type_error loc ty_actual ty_expected =
  error loc (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)  (* Map: String -> 'a *)

(* 3 environnements pour stocker
     les variables avec leur type,
     les fonctions avec leur signature
     les structures avec leurs champs
*)
    
type tenv = typ Env.t
type fenv = (typ list) * (typ list) Env.t
type senv = (ident * typ) list

let dummy = "_"

let add_env l tenv =
  (* Ajoute les types de variables dans tenv *)
  List.fold_left (fun env (x, t) -> if x = dummy then env else Env.add x t env) tenv l

let prog (fmt, ld) =
  (* collecte les noms des fonctions et des structures sans les vérifier *)
  let (fenv, senv) =
    (* Met dans fenv et dans senv les declarations de fonctions et de structures *)
    List.fold_left
      (fun (fenv, senv) d ->
         match d with Struct(s) ->  (fenv, Env.add s.sname.id s.fields senv)
                    | Fun(f)   -> (Env.add f.fname.id (List.map snd f.params, f.return) fenv, senv)) 
                    (*TO CHECK*)
      (Env.empty, Env.empty) ld
  in
  
  let check_typ t =
    (* Verifie que le type existe *)
    match t with
    | TInt -> ()
    | TBool -> ()
    | TString -> ()
    | TStruct sname -> if Env.find_opt sname senv<>None then () else failwith ("type non implementé")
    (* TO CHECK *)
  in
  let check_fields lf = 
    (* Verifie que les types des champs de lf existent *)
    List.iter (fun s1 -> check_typ(snd s1)) lf
  (* TO CHECK *)
  in

  let type_field s champs =
    let fields_o = Env.find_opt s senv in
    match fields_o with
    | None -> failwith (Format.sprintf "The structure %s hasn't been declared" s)
    | Some fields -> List.assoc champs fields
  in
  
  (* Verifie que le type de e est bien typ *)
  let rec check_expr e typ tenv =  
    if e.edesc = Nil then 
      match typ with
      | TStruct _ -> ()
      | _ -> failwith(Format.sprintf "cannot use nil as %s value" (typ_to_string typ))
    else let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error e.eloc typ_e typ
  (* TO CHECK *)

  and type_expr e tenv = match e.edesc with
    | Int _  -> TInt
    | Bool _ -> TBool
    | String _ -> TString
    | Unop (op, ex) -> type_expr_unop ex op tenv
    | Binop (op, e1, e2) -> type_expr_binop e1 e2 op tenv
    | Var v -> type_expr_var v tenv
    | Dot (ex, champs) -> type_expr_dot ex champs tenv
    | Nil -> failwith "this case shall not be used in type_expr";
    | New s -> TStruct(s)
    | Call (func, exprs) -> type_expr_call func.id exprs tenv (* Il faut renvoyer les éléments de retour *)
    | Print _ -> TStruct("")  (* Il ne devrait avoir aucun type *)

  and type_expr_call func exprs tenv =
    let types_o = Env.find_opt func fenv in
    match types_o with 
    | None -> failwith (Format.sprintf "the function %s doesn't exist" func)
    | Some types -> well_formed_arguments (fst types) exprs tenv

  and well_formed_arguments argument_types expressions tenv =
    let f t e =
       let actual = type_expr e tenv in
       if actual <> t then type_error e.eloc actual t
    in
    try 
      let () = List.iter2 f argument_types expressions in
      TInt
    with Invalid_argument _ -> failwith "The function has not been called with the right number of arguments" 

  and type_expr_dot e champs tenv = 
    let t = type_expr e tenv in
    match t with
    | TStruct s -> type_field s champs
    | _ -> type_error e.eloc t (TStruct "?")

  and type_expr_binop e1 e2 op tenv = 
    match op with
        | Eq | Neq -> if (e1.edesc <> Nil || e2.edesc <> Nil) && (type_expr e1 tenv) = (type_expr e2 tenv) then TBool
                      else failwith "a == b works only if a and b have the same type"
        | Gt | Ge | Lt | Le -> 
          let _ = check_expr e1 TInt tenv in
          let _ = check_expr e2 TInt tenv in
          TBool
        | Add | Sub | Mul | Div | Rem -> 
          let _ = check_expr e1 TInt tenv in
          let _ = check_expr e2 TInt tenv in
          TInt
        | Or | And -> 
          let _ = check_expr e1 TBool tenv in
          let _ = check_expr e2 TBool tenv in
          TBool

  and type_expr_var v tenv = 
    let t = Env.find_opt v.id tenv in
    if t = None then failwith (Format.sprintf "The variable %s doesn't exist" v.id)
    else Option.get t

  and type_expr_unop e op tenv =
    match op with
        | Opp -> 
          let () = check_expr e TInt tenv in 
          TInt
        | Not -> 
          let () = check_expr e TBool tenv in 
          TBool
  in

  let rec check_instr i ret tenv = match i.idesc with
    | _ -> failwith "case not implemented in check_instr"
  and check_seq s ret tenv =
    List.iter (fun i -> check_instr i ret tenv) s
  in
  
  let check_function f = failwith "case not implemented in check_function"

  in Env.iter (fun _ lf -> check_fields lf) senv;
     Env.iter (fun _ fd -> check_function fd) fenv


