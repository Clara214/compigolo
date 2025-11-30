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
type senv = (ident * typ) list Env.t

let dummy = "_"

let add_env env (x, t) =
  if x = dummy then env else Env.add x t env

let prog (fmt, ld) =
  (* collecte les noms des fonctions et des structures sans les vérifier *)
  let (fenv, senv) =
  (* Met dans fenv et dans senv les declarations de fonctions et de structures *)
    List.fold_left
      (fun (fenv, senv) d ->
        match d with
          Struct(s) ->  (fenv, Env.add s.sname.id s.fields senv)
          | Fun(f)   -> (Env.add f.fname.id (List.map snd f.params, f.return) fenv, senv)) 
      (Env.empty, Env.empty) ld
  in
  
  let check_typ t =
    (* Verifie que le type existe *)
    match t with
    | TInt -> ()
    | TBool -> ()
    | TString -> ()
    | TStruct sname -> if Env.find_opt sname senv<>None then () else failwith ("type non implementé")
  in

  let check_fields lf = 
    (* Verifie que les types des champs de lf existent *)
    List.iter (fun s1 -> check_typ(snd s1)) lf
  in

  let type_field s champs =
    let fields_o = Env.find_opt s senv in
    match fields_o with
    | None -> failwith (Format.sprintf "The structure %s hasn't been declared" s)
    | Some fields -> try List.assoc champs fields with Not_found -> failwith "champ pas trouvé"
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
  
  and check_expr_binop e1 e2 t1 t2 =
    match t1, t2, e1.edesc, e2.edesc with
            (* MODIF: correction syntaxe 'when'. On compare t1_ et t2_ extraits du Some *)
            | Some t1_, Some t2_, _, _ when t1_ = t2_ -> TBool 
            | _, Some (TStruct _), Nil, _ -> TBool (* nil = struct *)
            | Some (TStruct _), _, _, Nil -> TBool (* struct = nil *)
            | _, _, Nil, Nil -> failwith "on compare pas nil avec nil"
            | _ -> failwith "deux expr pas de meme type"
  
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
    | Call (func, exprs) -> type_expr_call func.id exprs tenv 
    | Print _ -> []

  and type_expr_call func exprs tenv =
    let types_o = Env.find_opt func fenv in
    match types_o with 
    | None -> failwith (Format.sprintf "the function %s doesn't exist" func)
    | Some types -> 
      well_formed_arguments (fst types) exprs tenv;
      snd types

  and well_formed_arguments argument_types expressions tenv =
    let type_exprs = List.flatten (List.map(fun e -> type_expr e tenv) expressions) in
    try 
      List.iter2 (fun e1 e2 -> 
        if e1 <> e2 then failwith "le type d'un des arguments n'est pas celui attendu "
      ) argument_types type_exprs
    with Invalid_argument _ -> failwith "nombre incorrect d'arguments"


  and type_expr_dot e champs tenv = 
    let types = type_expr e tenv in
    let t = get_unique_type types in
    match t with
      | TStruct s -> type_field s champs
      | _ -> type_error e.eloc t (TStruct "?")

  and type_expr_binop e1 e2 op tenv = 
    match op with
        | Eq | Neq -> let t1 = try Some (get_unique_type (type_expr e1 tenv)) with _ -> None in 
                      let t2 = try Some (get_unique_type (type_expr e2 tenv)) with _ -> None in
                    
          check_expr_binop e1 e2 t1 t2
        | Gt | Ge | Lt | Le -> 
          check_expr e1 TInt tenv;
          check_expr e2 TInt tenv;
          TBool
        | Add | Sub | Mul | Div | Rem -> 
          check_expr e1 TInt tenv;
          check_expr e2 TInt tenv;
          TInt
        | Or | And -> 
          check_expr e1 TBool tenv;
          check_expr e2 TBool tenv;
          TBool

  and type_expr_var v tenv = 
    let t = Env.find_opt v.id tenv in
    if t = None then failwith (Format.sprintf "The variable %s doesn't exist" v.id)
    else Option.get t

  and type_expr_unop e op tenv =
    match op with
        | Opp -> check_expr e TInt tenv; TInt
        | Not ->  check_expr e TBool tenv; TBool
          
  in 

  (*----------------------------------------Fonctions pour check_instr------------------------------------------------------*)

  let is_expr_gauche e =
    match e with
    | Var _ -> true
    | Dot _  -> true
    | _ -> false
  in

  let check_eq tenv e1 e2 =
    if not(is_expr_gauche e1.edesc) then failwith "e1 n'est pas une expression gauche";
    match e2.edesc with
      | Nil -> 
          let t1 = get_unique_type (type_expr e1 tenv) in
          (match t1 with TStruct _ -> () | _ -> failwith "nil affecté à un non pointeur")
      | _ -> 
        let t1 = get_unique_type (type_expr e1 tenv) in
        check_expr e2 t1 tenv 
 in


  let check_vars tenv il t el =
    List.iter (fun v -> if v.id <> dummy && Env.mem v.id tenv then failwith "cette variable a déjà été déclarée" ) il;
    let types_expr = 
      match el with
      | [] -> [] (* Pas d'init *)
      | _ -> List.flatten (List.map (fun e -> type_expr e tenv) el)
    in

    if el <> [] && List.length il <> List.length types_expr then
      failwith "Nombre de variables pas égal au nombre de valeurs";
    match t with
    | Some type_decla ->
        (* Si un type est déclaré explicitement *)
        check_typ type_decla;
        if el <> [] then
          List.iter (fun actual_t -> 
             if actual_t <> type_decla then failwith "les types ne sont pas cohérents"
          ) types_expr;
        List.fold_left (fun env x -> add_env env (x.id, type_decla)) tenv il

    | None ->
        if el = [] then failwith "les var doivent soit etre init soit avoir un type";
        List.fold_left2 (fun env x typ -> add_env env (x.id, typ)) tenv il types_expr
  in

  let check_set l1 l2 tenv = 
    let types_droit = List.flatten (List.map (fun e -> type_expr e tenv) l2) in
          if List.length l1 <> List.length types_droit then failwith "nombre dexpr à droite diff du nombre d'expr à gauche";
          List.iter2 (fun e_g t_r -> 
             if not (is_expr_gauche e_g.edesc) then failwith "Not a left value";
             let t_left = get_unique_type (type_expr e_g tenv) in
             if t_left <> t_r then 
               (match t_left, t_r with 
                | TStruct _, _ -> ()  
                | _ -> if t_left <> t_r then failwith "types pas pareil")
          ) l1 types_droit
  in

  let check_return el ret tenv =
    let lt = List.flatten (List.map (fun e -> type_expr e tenv) el) in
          if List.length lt <> List.length ret then failwith "nombre delements renvoyés incorrect";
    List.iter2 (fun e1 e2 -> if e2 <> e1 then failwith "pas bon return type") lt ret
  in


  let rec check_instr i ret tenv = 
    (*note : on veut changer l'environnement dans check_vars mais on veut renvoyer un unit d'où le ignore*)
    match i.idesc with
      | Inc e|Dec  e -> 
        if is_expr_gauche e.edesc then check_expr e TInt tenv
        else failwith "this is NOT a left expression"
      | If (e, s1, s2) -> (check_expr e TBool tenv);(check_seq s1 ret tenv); check_seq s2 ret tenv;
      | For(e,s) -> check_expr e TBool tenv; check_seq s ret tenv;
      | Block(s) -> check_seq s ret tenv
      | Set(l1,l2) -> check_set l1 l2 tenv
      | Expr(e) -> ignore(List.iter check_typ (type_expr e tenv))
      | Pset(_,le) -> List.iter (fun e -> List.iter check_typ (type_expr e tenv)) le
      | Vars(il,t,el) -> ignore(check_vars tenv il t el)
      | Return(el) -> check_return el ret tenv
      | _ -> failwith "Instruction pas définie"

  and check_seq s ret tenv =
    let _ = List.fold_left (fun env i -> 
       match i.idesc with
       | Vars(il, t, el) -> check_vars env il t el (*la portée des vars locales se gere ici récursivement avec la ligne d'en dessous !! faire un petit arbre pour comprendre *)
       | _ -> check_instr i ret env; env
    ) tenv s in ()
  in
  
  let check_function f = 
    let params_env = List.fold_left (fun env (v, t) -> 
        check_typ t;
        if Env.mem v.id env then failwith "Deux param ont le mm nom";
        Env.add v.id t env
    ) Env.empty f.params in
    
    check_seq f.body f.return params_env;
  in
  
  Env.iter (fun _ lf -> check_fields lf) senv;
  
  (* Vérification du corps des fonctions *)
  List.iter (fun d -> 
    match d with 
    | Fun f -> check_function f 
    | _ -> ()
  ) ld;
  (* Vérification du main *)
  let _ = match Env.find_opt "main" fenv with
  | Some ([], []) -> () (* main : void -> void *)
  | Some _ -> failwith "main must take no arguments and return nothing"
  | None -> failwith "main function missing" in
  ld