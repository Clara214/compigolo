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
  | Int _ | Bool _ | String _ -> VSet.empty
  | Var x -> VSet.singleton x.id
  | Binop(_, e1, e2) -> 
    VSet.union (vars_expr e1.edesc) (vars_expr e2.edesc)
  | Unop(_, e) -> vars_expr e.edesc
  | Print el -> List.fold_left (fun acc e -> VSet.union acc (vars_expr e.edesc)) VSet.empty el 
  | _ -> failwith "not implemented in vars_expr"
let rec vars_instr = function
  | Set(x, e) -> failwith "not implemented"
    (* List.fold_left2 (fun acc x e -> VSet.union acc (VSet.add x (vars_expr e.edesc))) VSet.empty x e *)
    (* On doit coder les structures avant pour faire un truc bien ici *)
  | For(e, s) -> 
     VSet.union (vars_expr e.edesc) (vars_seq s)
  | If(e, s1, s2) -> 
     VSet.union (vars_expr e.edesc) 
       (VSet.union (vars_seq s1) (vars_seq s2))
  | _ -> failwith "not implemented in vars_instr"
and vars_seq = function
  | []   -> VSet.empty
  | i::s -> VSet.union (vars_instr i.idesc) (vars_seq s)

let tr_prog p =
  let text = tr_seq p in
  let vars = vars_seq p in
  let data = VSet.fold 
      (fun id code -> label id @@ dword [0] @@ code) 
      vars nop 
  in
  { text; data }


  let file declarations =

    let create_tab_activations =
      let get_var_body b =
        let tmp = List.map (fun v -> 
        match v.idesc with
        Vars(il,_,_) -> (List.map(fun t -> t.id) il) 
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
        | Struct _ -> acc
        | Fun f -> Env.add f.fname.id ((get_var_params f.params)@(get_var_body f.body), List.length f.return) acc
      in     

      List.fold_left decl_to_env Env.empty declarations

    in
    (* crée l'environnement des structures*)
    let initialize_struct_env declarations =
      let decl_to_env acc decl =
        match decl with 
        | Fun _ -> acc
        | Struct s_def -> Env.add s_def.sname.id (List.map (fun e -> (fst e).id) s_def.fields) acc
      in
      List.fold_left decl_to_env Env.empty declarations
    in
    let senv = initialize_struct_env declarations in
    let fenv = create_tab_activations  in


    let get_var fname var_name =
      (*on suppose que noms de struct et func diff*)
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
      


    (*A ce stade là on a structure_env : classique
    et une liste de tableaux d'activations
    un tableau d'activation : ra,rp / variables locales/params/return
    *)

    (*generation du code mips*)
      
    let rec tr_expr f = function
    | Int(n)  -> li t0 (Int64.to_int n)
    | Bool b -> if b then li t0 1 else li t0 0
    | String s -> failwith "Nécessite une petite reflexion"
    | Var(id) -> li t0 (get_var f.fname.id id.id) 
    (*load l'adresse de id.id dans t0,  *)
    | Binop(bop, e1, e2) ->
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
      tr_expr e2.edesc
      @@ push t0
      @@ tr_expr e1.edesc
      @@ pop t1
      @@ op t0 t0 t1 
    | Unop(uop, e) ->
      let op = match uop with 
        | Not -> (fun r1 r2 -> xori r1 r2 1)
        | Opp -> (fun r1 r2 -> sub r1 zero r2)
      in
      tr_expr e.edesc
      @@ op t0 t0
    | Print(el) ->
      List.fold_left (fun acc e -> acc @@ tr_expr e.edesc @@ move a0 t0 @@ li v0 11 @@ syscall) Nop el
    | Nil -> li t0 0
    | New s -> failwith "Nécessite une petite réflexion"
    | _ -> failwith "not implemented in tr_expr"
    in
  let new_label =
    let cpt = ref (-1) in
    fun () -> incr cpt; Printf.sprintf "_label_%i" !cpt
  in
  let rec tr_seq = function
    | []   -> nop
    | [i]  -> tr_instr i
    | i::s -> tr_instr i @@ tr_seq s

  and tr_instr intr = 
    match intr.idesc with
    | Set(idl, el) -> failwith "not implemented"
      (*List.fold_left2 (fun acc id e -> acc @@ tr_expr e.edesc @@ la t1 id @@ sw t0 0 t1) Nop idl el *)
      (* On doit coder les structures avant pour faire un truc bien ici *)

    | If(c, s1, s2) ->
      let then_label = new_label()
      and end_label = new_label()
      in
      tr_expr c.edesc
      @@ bnez t0 then_label
      @@ tr_seq s2
      @@ b end_label
      @@ label then_label
      @@ tr_seq s1
      @@ label end_label

    | For(c, s) ->
      let test_label = new_label()
      and code_label = new_label()
      in
      b test_label
      @@ label code_label
      @@ tr_seq s
      @@ label test_label
      @@ tr_expr c.edesc
      @@ bnez t0 code_label
    | _ -> failwith "not implemented in tr_instr"
    in


    (*string -> (string list) env (new_struct-> champs)*)

    failwith "Not finished yet"

    
