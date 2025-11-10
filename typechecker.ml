open Mgoast

exception Error of Mgoast.location * string
let error loc s = raise (Error (loc,s))

let type_error loc ty_actual ty_expected =
  error loc (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)

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
  List.fold_left (fun env (x, t) -> if x = dummy then env else Env.add x t env) tenv l

let prog (fmt,ld) =
  (* collecte les noms des fonctions et des structures sans les vérifier *)
  let (fenv,senv) =
    List.fold_left
      (fun (fenv,senv) d ->
         match d with Struct(s) -> (fenv, Env.add s.sname.id s.fields senv)
                    | Fun(f)   -> failwith "à compléter")
      (Env.empty, Env.empty) ld
  in
  let check_typ t = failwith "case not implemented in check_typ"
  in
  let check_fields lf = failwith "case not implemented in check_fields"
  in
  let rec check_expr e typ tenv =
    if e.edesc = Nil then  failwith "case not implemented in check"
    else let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error e.eloc typ_e typ
  and type_expr e tenv = match e.edesc with
    | Int _  -> TInt
    | _ -> failwith "case not implemented in type_expr"

  in

  let rec check_instr i ret tenv = match i.idesc with
    | _ -> failwith "case not implemented in check_instr"
  and check_seq s ret tenv =
    List.iter (fun i -> check_instr i ret tenv) s
  in
  
  let check_function f = failwith "case not implemented in check_function"

  in Env.iter (fun _ lf -> check_fields lf) senv;
     Env.iter (fun _ fd -> check_function fd) fenv


