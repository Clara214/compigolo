type asm =
    | Nop
    | S of string
    | C of asm * asm

let (@@) x y = C (x, y)

type program = { text: asm; data: asm; }

let t0 = "$t0"
let t1 = "$t1"
let a0 = "$a0"
let v0 = "$v0"
let sp = "$sp"
let ra = "$ra"

open Printf
let li   r1 i      = S(sprintf "  li   %s, %i"     r1 i)
let la   r1 x      = S(sprintf "  la   %s, %s"     r1 x)
let move r1 r2     = S(sprintf "  move %s, %s"     r1 r2)

let add  r1 r2 r3  = S(sprintf "  add  %s, %s, %s" r1 r2 r3)
let addi r1 r2 i   = S(sprintf "  addi %s, %s, %d" r1 r2 i)
let mul  r1 r2 r3  = S(sprintf "  mul  %s, %s, %s" r1 r2 r3)
let slt  r1 r2 r3  = S(sprintf "  slt  %s, %s, %s" r1 r2 r3)
let and_ r1 r2 r3  = S(sprintf "  and  %s, %s, %s" r1 r2 r3)

let j    l         = S(sprintf "  j    %s"         l)
let jal  l         = S(sprintf "  jal  %s"         l)
let jr   r1        = S(sprintf "  jr   %s"         r1)
let jalr r1        = S(sprintf "  jalr  %s"        r1)
let b    l         = S(sprintf "  b    %s"         l)
let beqz r1 l      = S(sprintf "  beqz %s, %s"     r1 l)
let bnez r1 l      = S(sprintf "  bnez %s, %s"     r1 l)
let bltz r1 l      = S(sprintf "  bltz %s, %s"     r1 l)
let bge  r1 r2 l   = S(sprintf "  bge  %s, %s, %s" r1 r2 l)

let syscall         = S("  syscall")
let nop             = Nop
let label l         = S(sprintf "%s:" l)
let comment s       = S(sprintf "  # %s" s)

let lw   r1 o r2   = S(sprintf "  lw   %s, %d(%s)" r1 o r2)
let sw   r1 o r2   = S(sprintf "  sw   %s, %d(%s)" r1 o r2)
let lbu  r1 o r2   = S(sprintf "  lbu  %s, %d(%s)" r1 o r2)

let rec ilist = function
    | []     -> ""
    | [i]    -> sprintf "%d" i
    | i :: l -> sprintf "%d, %s" i (ilist l)
let dword l  = S(sprintf "  .word %s" (ilist l))
let asciiz s = S(sprintf "  .asciiz %s" s)

let push r =
    addi sp sp (-4) @@ sw r 0(sp)
let pop r =
    lw r 0(sp) @@ addi sp sp 4

let rec print_asm fmt a =
    match a with
    | Nop        -> ()
    | S s        -> fprintf fmt "%s\n" s
    | C (a1, a2) ->
     let () = print_asm fmt a1 in
       print_asm fmt a2

let print_program fmt p =
    fprintf fmt ".text\n";
    print_asm fmt p.text;
    fprintf fmt ".data\n";
    print_asm fmt p.data


