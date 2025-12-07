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

let prog (_, ld) =
  (* collecte les noms des fonctions et des structures sans les vérifier *)
  let check_already_declared fenv senv name =
    if Env.mem name fenv || Env.mem name senv then failwith (Format.sprintf "%s a ete declare plusieurs fois" name)
  in

  let (fenv, senv) =
  (* Met dans fenv et dans senv les declarations de fonctions et de structures *)
    List.fold_left
      (fun (fenv, senv) d ->
        match d with
          Struct(s) -> let () = check_already_declared fenv senv s.sname.id in (fenv, Env.add s.sname.id s.fields senv)
          | Fun(f)  -> let () = check_already_declared fenv senv f.fname.id in (Env.add f.fname.id (List.map snd f.params, f.return) fenv, senv)) 
      (Env.empty, Env.empty) ld
  in
  
  let check_typ t =
    (* Verifie que le type existe *)
    match t with
    | TInt -> ()
    | TBool -> ()
    | TString -> ()
    | TStruct sname -> if Env.find_opt sname senv<>None then () else failwith (Format.sprintf "type %s non implemente" sname)
  in

  let check_fields lf = 
    (* Verifie que les types des champs de lf existent *)
    List.iter (fun s1 -> check_typ(snd s1)) lf
  in

  let type_field s champs =
    let fields_o = Env.find_opt s senv in
    match fields_o with
    | None -> error champs.loc (Format.sprintf "La structure %s n'a jamais été déclarée" s)
    | Some fields -> 
      try List.assoc champs.id (List.map (fun e->((fst e).id, snd e)) fields) 
    with Not_found -> error champs.loc (Format.sprintf "Le champs %s n'existe pas" champs.id)
  in

  let get_unique_type lt loc =
    match lt with
    | [] -> error loc "L'expression n'a pas de type"
    | _::_::_ -> error loc "L'expression a plusieurs types, alors qu'il ne devrait en avoir qu'un seul"
    | t::_ -> t
  in
  
  
  
  (* Verifie que le type de e est bien typ *)
  let rec check_expr e typ tenv =  
    if e.edesc = Nil then 
      match typ with
      | TStruct _ -> {edesc_t=Nil_t; etype=None}
      | _ -> failwith(Format.sprintf "impossible d'utiliser un nil comme %s value" (typ_to_string typ))
    else let typ_e_, expr = type_expr e tenv in
    let typ_e = get_unique_type typ_e_ e.eloc in
    if typ_e <> typ then type_error e.eloc typ_e typ
    else expr
  
  and check_expr_binop e1 e2 t1 t2 loc =
    match t1, t2, e1.edesc, e2.edesc with
    (* MODIF: correction syntaxe 'when'. On compare t1_ et t2_ extraits du Some *)
    | Some t1_, Some t2_, _, _ when t1_ = t2_ -> TBool
    | _, Some (TStruct _), Nil, _ -> TBool (* nil = struct *)
    | Some (TStruct _), _, _, Nil -> TBool (* struct = nil *)
    | _, _, Nil, Nil -> error loc "On ne compare pas nil avec nil"
    | _ -> error loc "Impossible de faire une opération entre deux expressions de type différent"
  
  and type_expr (e: expr) (tenv: typ Env.t ) : typ list * expr_typed = match e.edesc with
    | Int i  -> [TInt], {edesc_t=Int_t i; etype=Some TInt}
    | Bool b -> [TBool], {edesc_t=Bool_t b; etype=Some TBool}
    | String s -> [TString], {edesc_t=String_t s; etype=Some TString}
    | Unop (op, ex) -> type_expr_unop ex op tenv
    | Binop (op, e1, e2) -> type_expr_binop e1 e2 op tenv e.eloc
    | Var v -> type_expr_var v tenv
    | Dot (ex, champs) -> type_expr_dot ex champs tenv
    | Nil -> failwith "Ce cas ne devrait pas être traité dans type_expr";
    | New s -> [TStruct(s)], {edesc_t=New_t s; etype=Some (TStruct s)}
    | Call (func, exprs) -> type_expr_call func exprs tenv func.loc 
    | Print el -> 
      let _, exprs = List.split(List.map (fun e -> type_expr e tenv) el) in
      [], {edesc_t=Print_t exprs; etype=None}

  and type_expr_call func exprs tenv loc =
    let types_o = Env.find_opt func.id fenv in
    match types_o with 
    | None -> error loc (Format.sprintf "La fonction %s n'existe pas" func.id)
    | Some types ->
      let exprs_t = well_formed_arguments (fst types) exprs tenv loc in
      if List.length (snd types) = 1 then
        snd types, {edesc_t=Call_t(func, exprs_t); etype=Some (List.hd (snd types))}
      else
        snd types, {edesc_t=Call_t(func, exprs_t); etype=None}

  and well_formed_arguments argument_types expressions tenv loc =
    let type_exprs_, exprs_t = List.split (List.map(fun e -> type_expr e tenv) expressions) in
    let type_exprs = List.flatten type_exprs_ in
    try 
      List.iter2 (fun e1 e2 -> 
        if e1 <> e2 then error loc "le type d'un des arguments n'est pas celui attendu "
      ) argument_types type_exprs;
      exprs_t
    with Invalid_argument _ -> error loc "nombre incorrect d'arguments"


  and type_expr_dot e champs tenv = 
    let types, e_t = type_expr e tenv in
    let t = get_unique_type types e.eloc in
    match t with
      | TStruct s -> 
        let t_ = type_field s champs in
        [t_], {edesc_t=Dot_t(e_t, champs); etype=Some t_}
      | _ -> type_error e.eloc t (TStruct "struct")

  and type_expr_binop e1 e2 op tenv loc = 
    match op with
        | Eq | Neq -> let t1 = try Some (get_unique_type (fst (type_expr e1 tenv)) e1.eloc) with _ -> None in 
                      let t2 = try Some (get_unique_type (fst (type_expr e2 tenv)) e2.eloc) with _ -> None in
          let _, e1_t = type_expr e1 tenv in
          let _, e2_t = type_expr e2 tenv in
          let t = check_expr_binop e1 e2 t1 t2 loc in
          ([t], {edesc_t=Binop_t(op, e1_t, e2_t); etype=Some t})
        | Gt | Ge | Lt | Le -> 
          let e1_t = check_expr e1 TInt tenv in
          let e2_t = check_expr e2 TInt tenv in
          [TBool], {edesc_t=Binop_t(op, e1_t, e2_t); etype=Some TBool}
        | Add | Sub | Mul | Div | Rem -> 
          let e1_t = check_expr e1 TInt tenv in
          let e2_t = check_expr e2 TInt tenv in
          [TInt], {edesc_t=Binop_t(op, e1_t, e2_t); etype=Some TInt}
        | Or | And -> 
          let e1_t = check_expr e1 TBool tenv in
          let e2_t = check_expr e2 TBool tenv in
          [TBool], {edesc_t=Binop_t(op, e1_t, e2_t); etype=Some TBool}

  and type_expr_var v tenv = 
    if v.id <> "_" then
      let t = Env.find_opt v.id tenv in
      if t = None then error v.loc (Format.sprintf "La variable %s n'existe pas" v.id)
      else [Option.get t], {edesc_t=Var_t(v); etype=t}
    else 
      [TStruct ""], {edesc_t=Var_t(v); etype=None}

  and type_expr_unop e op tenv =
    match op with
        | Opp -> let e_t = check_expr e TInt tenv in [TInt], {edesc_t=Unop_t(Opp, e_t); etype=Some TInt}
        | Not -> let e_t = check_expr e TBool tenv in [TBool], {edesc_t=Unop_t(Not, e_t); etype=Some TBool}
          
  in 

  (*----------------------------------------Fonctions pour check_instr------------------------------------------------------*)

  let is_expr_gauche e =
    match e with
    | Var _ -> true
    | Dot _  -> true
    | _ -> false
  in

  let check_vars tenv il t el loc =
    List.iter (fun v -> if v.id <> dummy && Env.mem v.id tenv then error v.loc "cette variable a déjà été déclarée" ) il;
    let types_expr_, exprs_t = 
      match el with
      | [] -> List.split([])
      | _ -> List.split (List.map (fun e -> type_expr e tenv) el)
    in
    let types_expr = List.flatten types_expr_ in
    let () = List.iter (fun t -> check_typ t) types_expr in
    if el <> [] && List.length il <> List.length types_expr then
      error loc "Le nombre de variables n'est pas égal au nombre de valeurs";
    match t with
    | Some type_decla ->
        (* Si un type est déclaré explicitement *)
        check_typ type_decla;
        if el <> [] then
          List.iter (fun actual_t -> 
             if actual_t <> type_decla then error loc "Les types ne sont pas cohérents"
          ) types_expr;
        (List.fold_left (fun env x -> add_env env (x.id, type_decla)) tenv il,
        Vars_t(il, t, exprs_t))

    | None ->
        if el = [] then error loc "Les var doivent soit etre initializees soit avoir un type";
        (List.fold_left2 (fun env x typ -> add_env env (x.id, typ)) tenv il types_expr,
        Vars_t(il, t, exprs_t))
  in

  let check_pset tenv il el loc =
    let check_card e =
      let types, exprs_t = type_expr e tenv in
      if List.length types <> 1 then error e.eloc "Il y a plusieurs types de retour alors qu'un seule est attendue"
      else types, exprs_t
    in
    let (types_expr, pset) =
      if List.length il = List.length el then (* a, b, c := e1, e2, e3 *)
        let aux1, aux2 = List.split(List.map check_card el) in
        let aux = List.flatten aux1 in
        let exprs_aux = aux2 in
        (aux, Pset_t (il, exprs_aux))
      else  (* a, b, c := f() *)
      if List.length el <> 1 then error loc "Ces expressions ont plusieurs types de retour, alors que dans ce contexte, elles ne devraient avoir qu'un type de retour"
      else
        let t, e_t = type_expr (List.hd el) tenv in
        (t, Pset_t (il, [e_t]))
    in

    let () = List.iter (fun t -> check_typ t) types_expr in

    let check_existing_var tenv i t =
        let elt = Env.find_opt i.id tenv in
        match elt with
        | None -> Env.add i.id t tenv
        | Some t_mem -> if t_mem <> t then error i.loc "La variable a déjà été déclarée avec un autre type"
                        else tenv
        in
      (List.fold_left2 check_existing_var tenv il types_expr, pset)
    (* Au vu de la grammaire, on ne peut pas avoir de liste vide. On peut donc oublier le premier cas*)

  in


  let check_set l1 l2 tenv loc = 
    let types_droit_, exprs_droit = List.split(List.map (fun e -> type_expr e tenv) l2) in
    let types_droit = List.flatten types_droit_ in
    if List.length l1 <> List.length types_droit then error loc "Le nombre d'expressions à droite est différent du nombre d'expressions à gauche";

    let exprs_gauche = List.map2 (fun e_g t_r -> 
        if not (is_expr_gauche e_g.edesc) then error e_g.eloc "N'est pas une expression gauche";
        let t_left_, expr_gauche = type_expr e_g tenv in
        let t_left = get_unique_type t_left_ e_g.eloc in
        if t_left <> t_r then 
          match t_left, t_r with 
          | TStruct _, _ -> expr_gauche
          | _ -> if t_left <> t_r then error e_g.eloc "Les types ne sont pas pareil"
                 else expr_gauche
        else
          expr_gauche
    ) l1 types_droit
        in
    Set_t (exprs_gauche, exprs_droit)
  in

  let check_return el ret tenv loc =
    let lt_, expr = List.split (List.map (fun e -> type_expr e tenv) el) in
    let lt = List.flatten lt_ in
          if List.length lt <> List.length ret then error loc "Le nombre d'elements renvoyés est incorrect";
    List.iter2 (fun e1 e2 -> if e2 <> e1 then error loc "Ce n'est pas le bon type de retour") lt ret;
    Return_t expr
  in


  let rec check_instr i ret tenv = 
    (*note : on veut changer l'environnement dans check_vars mais on veut renvoyer un unit d'où le ignore*)
    match i.idesc with
      | Inc e  -> 
        if is_expr_gauche e.edesc then let e_t = check_expr e TInt tenv in Inc_t e_t
        else error e.eloc "Ce n'est PAS une expression gauche"
      | Dec  e -> 
        if is_expr_gauche e.edesc then let e_t = check_expr e TInt tenv in Dec_t e_t
        else error e.eloc "Ce n'est PAS une expression gauche"
      | If (e, s1, s2) -> 
        let e_t = check_expr e TBool tenv in
        let s1_t = check_seq s1 ret tenv in 
        let s2_t = check_seq s2 ret tenv in
        If_t (e_t, s1_t, s2_t)
      | For(e,s) -> 
        let e_t = check_expr e TBool tenv in
        let s_t = check_seq s ret tenv in
        For_t (e_t, s_t)
      | Block(s) -> Block_t (check_seq s ret tenv)
      | Set(l1,l2) -> check_set l1 l2 tenv i.iloc
      | Expr(e) -> 
        let types, e_t = type_expr e tenv in
        ignore(List.iter check_typ types); 
        if List.length types = 1 then Expr_t(e_t)
        else Expr_t(e_t)
      | Pset(il,le) -> snd (check_pset tenv il  le i.iloc)
      | Vars(il,t,el) -> snd (check_vars tenv il t el i.iloc)
      | Return(el) -> check_return el ret tenv i.iloc

  and check_seq s ret tenv =
    let rec fold_left_and_map f acc l lacc =
      match l with 
      | [] -> lacc
      | e::l' -> 
        let func_res = f acc e in
        fold_left_and_map f (fst func_res) l' ((snd func_res)::lacc)
      in 
    let seq_typed = fold_left_and_map (fun env i -> 
       match i.idesc with
       | Vars(il, t, el) -> check_vars env il t el i.iloc
       | Pset(il,el)     -> check_pset env il el i.iloc
       | _ -> let instr_typed = check_instr i ret env in
       (env, instr_typed)
      ) tenv s [] in 
    seq_typed 
    in

  let rec check_return_in_seq seq =
    let check_return_instr b i =
      match i.idesc with 
      | Return _ -> true
      | If (_, s1, s2) -> b || ((check_return_in_seq s1) && (check_return_in_seq s2))
      | Block s -> b || check_return_in_seq s
      | _ -> b
    in
    List.fold_left check_return_instr false seq
  in

  (*note pour le rapport :le masquage des variables dans les blocs nest pas géré dans notre grammaire !!!!!!*)
  let check_function f = 
    let params_env = List.fold_left (fun env (v, t) -> 
        check_typ t;
        if Env.mem v.id env then error v.loc "Deux paramètres ont le même nom";
        Env.add v.id t env
    ) Env.empty f.params in
    
    let seq_typed = check_seq f.body f.return params_env in
    if (List.length f.return > 0) && not (check_return_in_seq f.body) then 
      failwith (Format.sprintf "La fonction %s n'a pas de return dans chacune de ses branches" f.fname.id)
    else 
      Fun_t {fname_t=f.fname; params_t=f.params; return_t=f.return; body_t=seq_typed}
  in
  
  Env.iter (fun _ lf -> check_fields lf) senv;
  
  (* Vérification du corps des fonctions *)
  let get_ast decls = 
    List.map (fun d -> 
      match d with 
      | Fun f -> check_function f 
      | Struct s -> Struct_t s
    ) decls;
  in
  (* Vérification du main *)
  let _ = match Env.find_opt "main" fenv with
  | Some ([], []) -> () (* main : void -> void *)
  | Some _ -> failwith "main ne devrait pas prendre d'arguments, et devrait ne rien renvoyer"
  | None -> failwith "Il n'y a aucune fonction main" in


  get_ast ld