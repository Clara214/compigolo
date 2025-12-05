open Mgoast
open Mips

module Env = Map.Make(String)
let string_env = ref Env.empty


let push reg =
  subi sp sp 4
  @@ sw reg 0 sp

let pop  reg =
  lw reg 0 sp
  @@ addi sp sp 4

let regs = [| t0; t1; t2; |]
let nb_regs = Array.length regs

let get_string_label s = 
  if Env.mem s !string_env then
    Env.find s !string_env
  else
    let count = Env.cardinal !string_env in
    let  label = Printf.sprintf "_str_%i" count in
    string_env := Env.add s label !string_env;
    label


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
  | Set_t(x, e) -> failwith "not implemented 1"
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
  
let concat_asm asm_list = 
  List.fold_left (fun acc asm -> acc @@ asm) Nop asm_list

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
        | Struct_t s_def -> Env.add s_def.sname.id (List.map (fun e -> ((fst e).id, snd e)) s_def.fields) acc
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
      let return_dec = snd function_infos in
      8 + 4 * return_dec + 4 * find_index (fst function_infos) var_name 0 
    in
      
    let get_struct_field sname field =
      let fields = fst (List.split(Env.find sname senv)) in
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

    let activation_table_length func_infos = 8 + snd func_infos * 4 + List.length (fst func_infos) * 4 in

    let new_label =
      let cpt = ref (-1) in
      fun () -> incr cpt; Printf.sprintf "_label_%i" !cpt
    in

    let rec tr_expr f = function
    | Int_t(n)  -> li t0 (Int64.to_int n)
    | Bool_t b -> if b then li t0 1 else li t0 0
    | String_t s -> let label = get_string_label s in
                    la t0 label

    | Var_t(id) -> lw t0 (-get_var f.fname_t.id id.id) fp  (* La pile est a l'envers !! *)
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
      concat_asm (List.map (print_in_asm f) el)
    | Nil_t -> li t0 0
    | New_t s -> 
        let champs = Env.find s senv in
        let taille  = List.length champs * 4 in
        li a0 taille      
        @@ li v0 9      (*sbrk*)
        @@ syscall      
        @@ move t0 v0
    | Dot_t (e1, field) -> 
      tr_expr f e1.edesc_t
      @@ (match e1.etype with
        | Some(TStruct s_name) -> 
            let idx = get_struct_field s_name field.id in
            lw t0 (idx * 4) t0
        | _ -> failwith "Erreur: Dot sur quelque chose qui n'est pas une structure")

    | Call_t (fname, el) -> 
      let func_infos = Env.find fname.id fenv in
      let rec put_args dec_acc exprs = 
        match exprs with
        | [] -> Nop
        | e :: l_next -> tr_expr f e.edesc_t @@ sw t0 dec_acc fp @@ put_args (dec_acc - 4) l_next
      in
      subi sp sp (activation_table_length func_infos)
      @@ put_args (-8 - snd func_infos * 4) el
      @@ jal fname.id

      (* Lors des affectations, si on voit un call parmi les expressions, on doit lui donner les adresses *)
    


    | _ -> failwith " 2"

    
  and print_in_asm f e =
      if e.etype = None then failwith "Fait un print bizarre"
      else 
        let typ = Option.get e.etype in
        match typ with
        | TInt -> tr_expr f e.edesc_t @@ move a0 t0 @@ li v0 1 @@ syscall
        | TBool -> tr_expr f e.edesc_t @@ move a0 t0 @@ li v0 1 @@ syscall
        | TString -> tr_expr f e.edesc_t @@ move a0 t0 @@ li v0 4 @@ syscall
        | TStruct s -> 
          let label_nil = new_label () in
          let label_end = new_label () in
          tr_expr f e.edesc_t 
          @@ li v0 11
          @@ beqz t0 label_nil 
          @@ li a0 (Char.code '&') 
          @@ syscall
          @@ li a0 (Char.code '{') 
          @@ syscall
          @@ print_in_asm_struct (snd (List.split (Env.find s senv)))
          @@ li a0 (Char.code '}')
          @@ syscall
          @@ b label_end
          @@ label label_nil
          @@ li a0 (Char.code '<') 
          @@ syscall
          @@ li a0 (Char.code 'n') 
          @@ syscall
          @@ li a0 (Char.code 'i') 
          @@ syscall
          @@ li a0 (Char.code 'l') 
          @@ syscall
          @@ li a0 (Char.code '>') 
          @@ syscall
          @@ label label_end

  and print_in_asm_struct s =
    (* Affiche l'intérieur de l'instance de type struct s, en supposant qu'on a dans t0 l'adresse de l'instance*)
      if List.length s = 0 then Nop
      else 
        let t = List.hd s in
        let s' = List.tl s in
        lw a0 0 t0
        @@ 
        (
          match t with
          | TInt -> li v0 1 @@ syscall
          | TBool -> li v0 1 @@ syscall
          | TString -> li v0 4 @@ syscall
          | TStruct s -> 
            let label_nil = new_label () in
            let label_end = new_label () in
            li v0 11
            @@ beqz a0 label_nil 
            @@ li a0 (Char.code '&') 
            @@ syscall
            @@ li a0 (Char.code '{') 
            @@ syscall
            @@ push t0
            @@ lw t0 0 t0
            @@ print_in_asm_struct (snd (List.split (Env.find s senv)))
            @@ pop t0
            @@ li a0 (Char.code '}') 
            @@ syscall
            @@ b label_end
            @@ label label_nil
            @@ li a0 (Char.code '<') 
            @@ syscall
            @@ li a0 (Char.code 'n') 
            @@ syscall
            @@ li a0 (Char.code 'i') 
            @@ syscall
            @@ li a0 (Char.code 'l') 
            @@ syscall
            @@ li a0 (Char.code '>') 
            @@ syscall
            @@ label label_end
        )
        @@ addi t0 t0 4
        @@ print_in_asm_struct s'
      in
  
  let rec tr_seq f = function
    | []   -> nop
    | [i]  -> tr_instr f i
    | i::s -> tr_instr f i @@ tr_seq f s

  and tr_instr f intr = 
    match intr with
  | Set_t(lvals, exprs) ->
    (*push une affectation *)
    let tr_assign_one lval expr =
      
      tr_expr f expr.edesc_t
      @@ push t0
      
      (*on calcule l'adresse de destination, stocké dans t0*)
      @@
       (match lval.edesc_t with
          | Var_t id ->
              (*(fp - offset) *)
              
              let off = get_var f.fname_t.id id.id in
              subi t0 fp off
              
          | Dot_t (e_struct, field) ->
              (*Adresse=ptr_struct + offset_champ *)
              
              (* Calculer l'adresse de la structure (le pointeur) *)
              tr_expr f e_struct.edesc_t
              (* t0 contient l'adresse de base *)
              
              (* b. Ajouter l'offset du champ *)
              @@ (match e_struct.etype with
                  | Some(TStruct s_name) ->
                      let idx = get_struct_field s_name field.id in
                      addi t0 t0 (idx * 4) (* t0 pointe maintenant exactement sur le champ *)
                  | _ -> failwith "Set sur un champ de non-structure")
                  
          | _ -> failwith "Assignation impossible (pas une lvalue valide)"
         )
         
      (* 4. Faire l'écriture effective *)
      (* La pile contient la VALEUR. t0 contient l'ADRESSE. *)
      @@ pop t1       (* Récupérer la valeur calculée en (1) dans t1 *)
      @@ sw t1 0 t0   (* Écrire t1 à l'adresse pointée par t0 *)
    in
    
    (* Appliquer cela pour chaque paire (lval, expr) *)
    List.fold_left2 (fun acc lv e -> acc @@ tr_assign_one lv e) nop lvals exprs
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
    | Block_t b -> tr_seq f b
    | Return_t ret -> 
      let rec return_exprs exprs dec =
        match exprs with
        | [] -> Nop
        | e :: l_next -> tr_expr f e.edesc_t @@ lw t1 dec fp @@ sw t0 0 t1 @@ return_exprs l_next (dec-4)
      in
      let func_infos = Env.find f.fname_t.id fenv in
      return_exprs ret (-8)
      @@ j ("func_end_" ^ f.fname_t.id)
    | Expr_t e -> tr_expr f e.edesc_t
    | Vars_t (il, _, el) ->
      if el = [] then
        Nop
      else 
        Nop
    | _ -> failwith "not implemented 3"
    in

  let tr_prog f =
    let text = label f.fname_t.id
               @@ sw ra 8 sp
               @@ sw fp 4 sp
               @@ addi fp sp (activation_table_length (Env.find f.fname_t.id fenv))
               @@ tr_seq f f.body_t
               @@ label ("func_end_" ^ f.fname_t.id) 
               @@ lw ra 8 fp
               @@ lw fp 4 fp
               @@ addi sp sp (activation_table_length (Env.find f.fname_t.id fenv))
               @@ jr ra in
  
               
  { text; data = Nop}
  
in

  let apply_prog acc decl =
    match decl with 
    | Fun_t f -> 
      let prog = tr_prog f in
      {text=acc.text @@ prog.text; data=acc.data @@ prog.data}
    | Struct_t _ -> acc
  in
  List.fold_left apply_prog {text=j "main"; data=Nop} declarations 

    (*string -> (string list) env (new_struct-> champs)*)



    
