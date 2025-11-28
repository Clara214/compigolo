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

  let get_unique_type lt =
    match lt with
    | [] -> failwith "l'expression n'a pas de type"
    | _::_::_ -> failwith "l'expression a trop de types"
    | t::_ -> t
  in
  
  (* Verifie que le type de e est bien typ *)
  let rec check_expr e typ tenv =  
    if e.edesc = Nil then 
      match typ with
      | TStruct _ -> ()
      | _ -> failwith(Format.sprintf "cannot use nil as %s value" (typ_to_string typ))
    else let typ_e = get_unique_type(type_expr e tenv) in
    if typ_e <> typ then type_error e.eloc typ_e typ
  (* TO CHECK *)

  
  and type_expr (e: expr) (tenv: typ Env.t ) : typ list = match e.edesc with
    | Int _  -> [TInt]
    | Bool _ -> [TBool]
    | String _ -> [TString]
    | Unop (op, ex) -> [type_expr_unop ex op tenv]
    | Binop (op, e1, e2) -> [type_expr_binop e1 e2 op tenv]
    | Var v -> [type_expr_var v tenv]
    | Dot (ex, champs) -> [type_expr_dot ex champs tenv]
    | Nil -> failwith "this case shall not be used in type_expr";
    | New s -> [TStruct(s)]
    | Call (func, exprs) -> type_expr_call func.id exprs tenv (* Il faut renvoyer les éléments de retour *)
    | Print _ -> []

  and type_expr_call func exprs tenv =
    let types_o = Env.find_opt func fenv in
    match types_o with 
    | None -> failwith (Format.sprintf "the function %s doesn't exist" func)
    | Some types -> 
      well_formed_arguments (fst types) exprs tenv;
      snd types

  and well_formed_arguments argument_types expressions tenv =
    let f t1 t2 = if t1 <> t2 then failwith "The parameter has not the good type" in
    let actuals = List.flatten (List.map (fun e -> type_expr e tenv) expressions) in
    try 
      let () = List.iter2 f argument_types actuals in
      ()
    with Invalid_argument _ -> failwith "The function has not been called with the right number of arguments" 

  and type_expr_dot e champs tenv = 
    let types = type_expr e tenv in
    let t = get_unique_type types in
    match t with
      | TStruct s -> type_field s champs
      | _ -> type_error e.eloc t (TStruct "?")

  and type_expr_binop e1 e2 op tenv = 
    match op with
        | Eq | Neq -> if (e1.edesc <> Nil || e2.edesc <> Nil) && (get_unique_type (type_expr e1 tenv)) = (get_unique_type (type_expr e2 tenv)) then TBool
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

  let is_expr_gauche e =
    match e with
    |Var _ -> true
    | Dot _  -> true
    | _ -> false
  in

  let check_eq tenv e1 e2 =
    if not(is_expr_gauche e1.edesc) then failwith "e1 n'est pas une expression gauche";
    if get_unique_type(type_expr e1 tenv)=get_unique_type(type_expr e2 tenv)  then failwith "le type de deux expressions n'est pas égal"
  in


  let check_expr_same_typ le tenv=

    let aux_expr_mono_type t =
      let b = List.fold_left(fun acc e -> match (type_expr e tenv) with | [a] -> acc&&(a==t)  | _ -> failwith "le type des expressions n'est pas une variable") true le in
      if not b then failwith "le type des expressions n'est pas le meme !"
    in

    let aux_call lt =
      if List.length lt <> List.length l then
      List.fold_left
    in

    match type_expr (List.hd le) tenv with
    | [] -> failwith "l'expression rentrée est d'arité 0 "
    | e :: [] ->  aux_expr_mono_type e 
    | l -> 
    
    in

  
  let check_vars tenv il t el =
    (*List.iter (check_typ t) tl à faire *)
    (*idée : on regarde si le type existe, si les variables ne sont pas déjà déclarées, si cest le cas on verifie que type = expr_type*)
    (*on regarde si ttes les variables ne sont pas déjà déclarées*)
    let b1 = List.fold_left (fun acc a -> acc && (Env.mem a.id tenv)) true il in
    if not b1 then failwith "les variables ont déjà été déclarées"
    else
    (*on regarde si le type est valide*)
    match t with
    | None -> List.iter (check_typ (get_unique_type (type_expr e tenv ))) el
    | Some tt -> check_typ tt
    in

    




  let rec check_instr i ret tenv = 
    (*type de retour attendu d'une instruction ?*)
    match i.idesc with
      |Inc e|Dec  e -> 
        if is_expr_gauche e.edesc then check_expr e TInt tenv
        else failwith "this is NOT a left expression"
      |If (e, s1, s2) -> (check_expr e TBool tenv); (check_seq s1 ret tenv); check_seq s2 ret tenv;
      |For(e,s) -> check_expr e TBool tenv  ; check_seq s ret tenv;
      |Block(s) -> check_seq s ret tenv
      |Set(l1,l2) -> List.iter2 (check_eq tenv) l1 l2 
      |Expr(e) -> List.iter check_typ (type_expr e tenv)  
      |Pset(_,le) -> List.iter (fun e -> List.iter check_typ (type_expr e tenv)) le
      |Vars(il,t,el) -> check_vars tenv il t el
      | _ -> ()
      (*return à faire*)







  and check_seq s ret tenv =
    List.iter (fun i -> check_instr i ret tenv) s
  in
  
  let check_function f = failwith "case not implemented in check_function"

  in Env.iter (fun _ lf -> check_fields lf) senv;
     Env.iter (fun _ fd -> check_function fd) fenv


