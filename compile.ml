open Mgoast
open Mips

module Env = Map.Make(String)

let push reg =
  subi sp sp 4
  @@ sw reg 0 sp

let pop  reg =
  lw reg 0 sp
  @@ addi sp sp 4

let regs = [| t0; t1; t2; |]
let nb_regs = Array.length regs


module VSet = Set.Make(String)
let rec vars_expr = function
  | Int_t _ | Bool_t _ | String_t _ -> VSet.empty
  | Var_t x -> VSet.singleton x.id
  | Binop_t(_, e1, e2) -> 
    VSet.union (vars_expr e1.edesc_t) (vars_expr e2.edesc_t)
  | Unop_t(_, e) -> vars_expr e.edesc_t
  | Print_t el -> List.fold_left (fun acc e -> VSet.union acc (vars_expr e.edesc_t)) VSet.empty el 
  | _ -> failwith "not implemented in vars_expr"
let rec vars_instr = function
  | Set_t(x, e) -> failwith "not implemented"
    (* List.fold_left2 (fun acc x e -> VSet.union acc (VSet.add x (vars_expr e.edesc))) VSet.empty x e *)
    (* On doit coder les structures avant pour faire un truc bien ici *)
  | For_t(e, s) -> 
     VSet.union (vars_expr e.edesc_t) (vars_seq s)
  | If_t(e, s1, s2) -> 
     VSet.union (vars_expr e.edesc_t) 
       (VSet.union (vars_seq s1) (vars_seq s2))
  | _ -> failwith "not implemented in vars_instr"
and vars_seq = function
  | []   -> VSet.empty
  | i::s -> VSet.union (vars_instr i) (vars_seq s)


  let file declarations =
    let create_tab_activations =
      let get_var_body b =
        let tmp = List.map (fun v -> 
        match v with
        Vars_t(il,_,_) -> (List.map(fun t -> t.id) il) 
        | _ -> []
        ) b
        in
        List.flatten tmp
      in 

      let get_var_params p = 
        List.map (fun v -> (fst v).id)  p
      in


      let decl_to_env acc decl =
        match decl with
        | Struct_t _ -> acc
        | Fun_t f -> Env.add f.fname_t.id ((get_var_params f.params_t)@(get_var_body f.body_t), List.length f.return_t) acc
      in     

      List.fold_left decl_to_env Env.empty declarations

    in
    (* crée l'environnement des structures*)
    let initialize_struct_env declarations =
      let decl_to_env acc decl =
        match decl with 
        | Fun_t _ -> acc
        | Struct_t s_def -> Env.add s_def.sname.id (List.map (fun e -> (fst e).id) s_def.fields) acc
      in
      List.fold_left decl_to_env Env.empty declarations
    in
    let senv = initialize_struct_env declarations in
    let fenv = create_tab_activations in


    let get_var fname var_name =
      (* Renvoie le décalage nécessaire pour obtenir la position de la variable sur la pile *)
      let function_infos = Env.find fname fenv in
      let rec find_index vars id acc =
        match vars with
        | [] -> failwith "pas trouvé la var"
        | v :: l' -> 
          if id = v then acc
          else find_index l' id (acc+1)
      in
      let dec = snd function_infos in
      find_index (fst function_infos) var_name 0 + 8 + 4 * dec
    in
      
    let get_struct_field sname field =
      let fields = Env.find sname senv in
      let rec find_index vars id acc =
        match vars with
        | [] -> failwith "pas trouvé la var"
        | v :: l' -> 
          if id = v then acc
          else find_index l' id (acc+1)
        in
      find_index fields field 0
    in


    (*A ce stade là on a structure_env : classique
    et une liste de tableaux d'activations
    un tableau d'activation : ra,rp / variables locales/params/return
    *)

    (*generation du code mips*)
      
    let rec tr_expr f = function
    | Int_t(n)  -> li t0 (Int64.to_int n)
    | Bool_t b -> if b then li t0 1 else li t0 0
    | String_t s -> failwith "Nécessite une petite reflexion"
    | Var_t(id) -> lw t0 (get_var f.fname.id id.id) fp
    (*load l'adresse de id.id dans t0,  *)
    | Binop_t(bop, e1, e2) ->
      let op = match bop with
        | Add -> add
        | Sub -> sub
        | Mul -> mul
        | Div -> div
        | Lt  -> slt
        (* x xor 1 devrait donner !x *)
        | Le  -> (fun r1 r2 r3 -> slt r1 r3 r2 @@ xori r1 r1 1)
        | Gt -> (fun r1 r2 r3 -> slt r1 r3 r2)
        | Ge -> (fun r1 r2 r3 -> slt r1 r2 r3 @@ xori r1 r1 1)
        (* x xor y donne 0 ssi x = y*)
        | Eq -> (fun r1 r2 r3 -> xor r1 r2 r3 @@ slt r1 zero r1)
        | Neq -> (fun r1 r2 r3 -> xor r1 r2 r3 @@ slt r1 zero r1 @@ xori r1 r1 1)
        | And -> and_
        | Or -> or_
        | Rem -> rem
        
      in
      tr_expr f e2.edesc_t
      @@ push t0
      @@ tr_expr f e1.edesc_t
      @@ pop t1
      @@ op t0 t0 t1 
    | Unop_t(uop, e) ->
      let op = match uop with 
        | Not -> (fun r1 r2 -> xori r1 r2 1)
        | Opp -> (fun r1 r2 -> sub r1 zero r2)
      in
      tr_expr f e.edesc_t
      @@ op t0 t0
    | Print_t(el) ->
      List.fold_left (fun acc e -> acc @@ tr_expr f e.edesc_t @@ move a0 t0 @@ li v0 11 @@ syscall) Nop el
    | Nil_t -> li t0 0
    | New_t s -> failwith "Nécessite une petite réflexion"
    | Dot_t (e1, field) -> 
      let e1_compute = tr_expr f e1.edesc_t in
      failwith "not terminated"
    | _ -> failwith "not implemented"
    in
  let new_label =
    let cpt = ref (-1) in
    fun () -> incr cpt; Printf.sprintf "_label_%i" !cpt
  in
  let rec tr_seq f = function
    | []   -> nop
    | [i]  -> tr_instr f i
    | i::s -> tr_instr f i @@ tr_seq f s

  and tr_instr f intr = 
    match intr with
    | Set_t(idl, el) -> failwith "not implemented"
      (*List.fold_left2 (fun acc id e -> acc @@ tr_expr e.edesc @@ la t1 id @@ sw t0 0 t1) Nop idl el *)
      (* On doit coder les structures avant pour faire un truc bien ici *)

    | If_t(c, s1, s2) ->
      let then_label = new_label()
      and end_label = new_label()
      in
      tr_expr f c.edesc_t
      @@ bnez t0 then_label
      @@ tr_seq f s2
      @@ b end_label
      @@ label then_label
      @@ tr_seq f s1
      @@ label end_label

    | For_t(c, s) ->
      let test_label = new_label()
      and code_label = new_label()
      in
      b test_label
      @@ label code_label
      @@ tr_seq f s
      @@ label test_label
      @@ tr_expr f c.edesc_t
      @@ bnez t0 code_label
    | _ -> failwith "not implemented in tr_instr"
    in

  let tr_prog f p =
    let text = tr_seq f p in
    let vars = vars_seq p in
    let data = VSet.fold 
        (fun id code -> label id @@ dword [0] @@ code) 
        vars nop 
    in
    { text; data }
  in


    (*string -> (string list) env (new_struct-> champs)*)

    failwith "Not finished yet"

    
