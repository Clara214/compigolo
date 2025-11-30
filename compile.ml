open Mgoast
open Mips

let push reg =
  subi sp sp 4
  @@ sw reg 0 sp

let pop  reg =
  lw reg 0 sp
  @@ addi sp sp 4

let regs = [| t0; t1; t2; |]
let nb_regs = Array.length regs

let rec tr_expr = function
  | Int(n)  -> li t0 (Int64.to_int n)
  | Var(id) -> la t0 id.id @@ lw t0 0(t0)
  | Binop(bop, e1, e2) ->
    let op = match bop with
      | Add -> add
      | Mul -> mul
      | Lt  -> slt
      | And -> and_
      | _ -> failwith "operation not implemented"
    in
    tr_expr e2.edesc
    @@ push t0
    @@ tr_expr e1.edesc
    @@ pop t1
    @@ op t0 t0 t1 
  | Print(el) ->
    List.fold_left (fun acc e -> acc @@ tr_expr e.edesc @@ move a0 t0 @@ li v0 11 @@ syscall) Nop el
  | _ -> failwith "not implemented in tr_expr"    

let new_label =
  let cpt = ref (-1) in
  fun () -> incr cpt; Printf.sprintf "_label_%i" !cpt

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
