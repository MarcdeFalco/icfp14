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

let fickle = [| MOV(Reg 0, Const 255); MOV(Reg 1, Const 0);
    MOV(Reg 2, Const 255); INC(Reg 2);
    JGT(7,IReg 2,Reg 0);
    MOV(Reg 0, IReg 2); MOV(Reg 1, Reg 2);
    JLT(3,Reg 2,Const 3); MOV(Reg 0, Reg 1);
    INT 0; INT 3; INT 6; INC(IReg 1); HLT |]


