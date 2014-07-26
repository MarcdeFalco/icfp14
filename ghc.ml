type arg = 
    | Reg of int | IReg of int
    | Const of int | Mem of int

type instr = 
    | MOV of arg * arg
    | INC of arg
    | DEC of arg
    | ADD of arg * arg
    | SUB of arg * arg
    | MUL of arg * arg
    | DIV of arg * arg
    | AND of arg * arg
    | OR of arg * arg
    | XOR of arg * arg
    | JLT of int * arg * arg
    | JEQ of int * arg * arg
    | JGT of int * arg * arg
    | INT of int
    | HLT

let alwaysdown = [| MOV(Reg 0, Const 2); INT 0; HLT |]
