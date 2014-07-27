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

let chase = [| 
   JGT(4,(Mem 10),Const 40);
MOV((Mem 0),Const 0);
MOV((Mem 1),Const 0);
JEQ(7,(Reg 0),(Reg 0));
INT(1);
MOV((Mem 0),(Reg 0));
MOV((Mem 1),(Reg 1));
INT(3);
INT(5);
MOV((Mem 2),(Reg 0));
MOV((Mem 3),(Reg 1));
JEQ(17,(Mem 0),(Mem 2));
JGT(15,(Mem 0),(Mem 2));
MOV((Reg 0),Const 1);
JEQ(21,(Reg 0),(Reg 0));
MOV((Reg 0),Const 3);
JEQ(21,(Reg 0),(Reg 0));
JGT(19,(Mem 1),(Mem 3));
MOV((Reg 0),Const 0);
JEQ(21,(Reg 0),(Reg 0));
MOV((Reg 0),Const 1);
INT(0);
INC ((Mem 10));
HLT |]

let random = [|
    JGT(4,Mem 0,Const 0); MOV(Mem 0, Const 138);
    INT 3; ADD(Mem 0, Reg 0);
    MOV(Reg 2,Mem 0); MOV(Reg 1, Reg 2);
    DIV(Reg 1,Const 5); MOV(Reg 0, Reg 1);
    DIV(Reg 1,Const 4); MUL(Reg 1,Const 4); SUB(Reg 0,Reg 1);
    INT 0; MUL(Mem 0,Const 19); ADD(Mem 0,Const 7); HLT |]
