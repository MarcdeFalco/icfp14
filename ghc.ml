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

let ghost1 = [|
    INT(3);
    MOV(Reg 7, Reg 0);
    INT(5);
    MOV(Reg 2, Reg 0);
    MOV(Reg 3, Reg 1);
    INT(3);
    INT(6);
    MOV(Reg 6, Reg 1);
    JEQ(34, Reg 0, Const 2);
    JGT(34, IReg 7, Const 20);
    JEQ(22, Reg 6, Const 2);
    MOV(Reg 0, Reg 2);
    MOV(Reg 1, Reg 3);
    DEC(Reg 1);
    INT(7);
    JEQ(22, Reg 0, Const 0);
    JEQ(18, Reg 0, Const 1);
    MOV(IReg 7, Const 255);
    INC(IReg 7);
    MOV(Reg 0, Const 0);
    INT(0);
    HLT;
    JEQ(60, Reg 6, Const 1);
    MOV(Reg 0, Reg 2);
    MOV(Reg 1, Reg 3);
    DEC(Reg 0);
    INT(7);
    JEQ(33, Reg 0, Const 0);
    JEQ(30, Reg 0, Const 1);
    MOV(IReg 7, Const 255);
    INC(IReg 7);
    MOV(Reg 0, Const 3);
    INT(0);
    HLT;
    INT(1);
	MOV(Reg 4, Reg 0);
	MOV(Reg 5, Reg 1);
	JEQ(57, Reg 2, Reg 4);
	JGT(48, Reg 2, Reg 4);
	JEQ(57, Reg 6, Const 3);
	MOV(Reg 0, Reg 2);
	MOV(Reg 1, Reg 3);
	INC(Reg 0);
	INT(7);
	JEQ(57, Reg 0, Const 0);
	MOV(Reg 0, Const 1);
	INT(0);
	HLT;
	JEQ(57, Reg 6, Const 1);
	MOV(Reg 0, Reg 2);
	MOV(Reg 1, Reg 3);
	DEC(Reg 0);
	INT(7);
	JEQ(57, Reg 0, Const 0);
	MOV(Reg 0, Const 3);
	INT(0);
	HLT;
	JEQ(76, Reg 3, Reg 5);
	JGT(68, Reg 3, Reg 5);
	JEQ(76, Reg 6, Const 0);
	MOV(Reg 0, Reg 2);
	MOV(Reg 1, Reg 3);
	INC(Reg 1);
	INT(7);
	JEQ(76, Reg 0, Const 0);
	MOV(Reg 0, Const 2);
	INT(0);
	HLT;
	JEQ(76, Reg 6, Const 2);
	MOV(Reg 0, Reg 2);
	MOV(Reg 1, Reg 3);
	DEC(Reg 1);
	INT(7);
	JEQ(76, Reg 0, Const 0);
	MOV(Reg 0, Const 0);
	INT(0);
	HLT|]

let ghost2 = [|
    INT(3);
    MOV(Reg 7, Reg 0);
    INT(5);
    MOV(Reg 2, Reg 0);
    MOV(Reg 3, Reg 1);
    INT(3);
    INT(6);
    MOV(Reg 6, Reg 1);
    JEQ(34, Reg 0, Const 2);
    JGT(34, IReg 7, Const 20);
    JEQ(22, Reg 6, Const 2);
    MOV(Reg 0, Reg 2);
    MOV(Reg 1, Reg 3);
    DEC(Reg 1);
    INT(7);
    JEQ(22, Reg 0, Const 0);
    JEQ(18, Reg 0, Const 1);
    MOV(IReg 7, Const 255);
    INC(IReg 7);
    MOV(Reg 0, Const 0);
    INT(0);
    HLT;
    JEQ(60, Reg 6, Const 3);
    MOV(Reg 0, Reg 2);
    MOV(Reg 1, Reg 3);
    INC(Reg 0);
    INT(7);
    JEQ(33, Reg 0, Const 0);
    JEQ(30, Reg 0, Const 1);
    MOV(IReg 7, Const 255);
    INC(IReg 7);
    MOV(Reg 0, Const 1);
    INT(0);
    HLT;
    INT(1);
    MOV(Reg 4, Reg 0);
	MOV(Reg 5, Reg 1);
	JEQ(57, Reg 2, Reg 4);
	JGT(48, Reg 2, Reg 4);
	JEQ(57, Reg 6, Const 3);
	MOV(Reg 0, Reg 2);
	MOV(Reg 1, Reg 3);
	INC(Reg 0);
	INT(7);
	JEQ(57, Reg 0, Const 0);
	MOV(Reg 0, Const 1);
	INT(0);
	HLT;
	JEQ(57, Reg 6, Const 1);
	MOV(Reg 0, Reg 2);
	MOV(Reg 1, Reg 3);
	DEC(Reg 0);
	INT(7);
	JEQ(57, Reg 0, Const 0);
	MOV(Reg 0, Const 3);
	INT(0);
	HLT;
	JEQ(76, Reg 3, Reg 5);
	JGT(68, Reg 3, Reg 5);
	JEQ(76, Reg 6, Const 0);
	MOV(Reg 0, Reg 2);
	MOV(Reg 1, Reg 3);
	INC(Reg 1);
	INT(7);
	JEQ(76, Reg 0, Const 0);
	MOV(Reg 0, Const 2);
	INT(0);
	HLT;
	JEQ(76, Reg 6, Const 2);
	MOV(Reg 0, Reg 2);
	MOV(Reg 1, Reg 3);
	DEC(Reg 1);
	INT(7);
	JEQ(76, Reg 0, Const 0);
	MOV(Reg 0, Const 0);
	INT(0);
	HLT|]

let ghost3 = [|
    INT(3);
    MOV(Reg 7, Reg 0);
    INT(5);
    MOV(Reg 2, Reg 0);
    MOV(Reg 3, Reg 1);
    INT(3);
    INT(6);
    MOV(Reg 6, Reg 1);
    JEQ(34, Reg 0, Const 2);
    JGT(34, IReg 7, Const 20);
    JEQ(22, Reg 6, Const 0);
    MOV(Reg 0, Reg 2);
    MOV(Reg 1, Reg 3);
    INC(Reg 1);
    INT(7);
    JEQ(22, Reg 0, Const 0);
    JEQ(18, Reg 0, Const 1);
    MOV(IReg 7, Const 255);
    INC(IReg 7);
    MOV(Reg 0, Const 2);
    INT(0);
    HLT;
    JEQ(69, Reg 6, Const 1);
    MOV(Reg 0, Reg 2);
    MOV(Reg 1, Reg 3);
    DEC(Reg 0);
    INT(7);
    JEQ(33, Reg 0, Const 0);
    JEQ(30, Reg 0, Const 1);
    MOV(IReg 7, Const 255);
    INC(IReg 7);
    MOV(Reg 0, Const 3);
    INT(0);
    HLT;
    INT(1);
    MOV(Reg 4, Reg 0);
	MOV(Reg 5, Reg 1);
	JEQ(57, Reg 2, Reg 4);
	JGT(48, Reg 2, Reg 4);
	JEQ(57, Reg 6, Const 3);
	MOV(Reg 0, Reg 2);
	MOV(Reg 1, Reg 3);
	INC(Reg 0);
	INT(7);
	JEQ(57, Reg 0, Const 0);
	MOV(Reg 0, Const 1);
	INT(0);
	HLT;
	JEQ(57, Reg 6, Const 1);
	MOV(Reg 0, Reg 2);
	MOV(Reg 1, Reg 3);
	DEC(Reg 0);
	INT(7);
	JEQ(57, Reg 0, Const 0);
	MOV(Reg 0, Const 3);
	INT(0);
	HLT;
	JEQ(76, Reg 3, Reg 5);
	JGT(68, Reg 3, Reg 5);
	JEQ(76, Reg 6, Const 0);
	MOV(Reg 0, Reg 2);
	MOV(Reg 1, Reg 3);
	INC(Reg 1);
	INT(7);
	JEQ(76, Reg 0, Const 0);
	MOV(Reg 0, Const 2);
	INT(0);
	HLT;
	JEQ(76, Reg 6, Const 2);
	MOV(Reg 0, Reg 2);
	MOV(Reg 1, Reg 3);
	DEC(Reg 1);
	INT(7);
	JEQ(76, Reg 0, Const 0);
	MOV(Reg 0, Const 0);
	INT(0);
	HLT|]
    
let ghost4 = [|
    INT(3);
    MOV(Reg 7, Reg 0);
    INT(5);
    MOV(Reg 2, Reg 0);
    MOV(Reg 3, Reg 1);
    INT(3);
    INT(6);
    MOV(Reg 6, Reg 1);
    JEQ(34, Reg 0, Const 2);
    JGT(34, IReg 7, Const 20);
    JEQ(22, Reg 6, Const 0);
    MOV(Reg 0, Reg 2);
    MOV(Reg 1, Reg 3);
    INC(Reg 1);
    INT(7);
    JEQ(22, Reg 0, Const 0);
    JEQ(18, Reg 0, Const 1);
    MOV(IReg 7, Const 255);
    INC(IReg 7);
    MOV(Reg 0, Const 2);
    INT(0);
    HLT;
    JEQ(69, Reg 6, Const 3);
    MOV(Reg 0, Reg 2);
    MOV(Reg 1, Reg 3);
    INC(Reg 0);
    INT(7);
    JEQ(33, Reg 0, Const 0);
    JEQ(30, Reg 0, Const 1);
    MOV(IReg 7, Const 255);
    INC(IReg 7);
    MOV(Reg 0, Const 1);
    INT(0);
    HLT;
    INT(1);
    MOV(Reg 4, Reg 0);
	MOV(Reg 5, Reg 1);
	JEQ(57, Reg 2, Reg 4);
	JGT(48, Reg 2, Reg 4);
	JEQ(57, Reg 6, Const 3);
	MOV(Reg 0, Reg 2);
	MOV(Reg 1, Reg 3);
	INC(Reg 0);
	INT(7);
	JEQ(57, Reg 0, Const 0);
	MOV(Reg 0, Const 1);
	INT(0);
	HLT;
	JEQ(57, Reg 6, Const 1);
	MOV(Reg 0, Reg 2);
	MOV(Reg 1, Reg 3);
	DEC(Reg 0);
	INT(7);
	JEQ(57, Reg 0, Const 0);
	MOV(Reg 0, Const 3);
	INT(0);
	HLT;
	JEQ(76, Reg 3, Reg 5);
	JGT(68, Reg 3, Reg 5);
	JEQ(76, Reg 6, Const 0);
	MOV(Reg 0, Reg 2);
	MOV(Reg 1, Reg 3);
	INC(Reg 1);
	INT(7);
	JEQ(76, Reg 0, Const 0);
	MOV(Reg 0, Const 2);
	INT(0);
	HLT;
	JEQ(76, Reg 6, Const 2);
	MOV(Reg 0, Reg 2);
	MOV(Reg 1, Reg 3);
	DEC(Reg 1);
	INT(7);
	JEQ(76, Reg 0, Const 0);
	MOV(Reg 0, Const 0);
	INT(0);
	HLT|]

let print_ghc gl =
    let reg = [|"a";"b";"c";"d";"e";"f";"g";"h"|] in
    let sod d = match d with
        Reg x -> reg.(x)
        | IReg x -> "[" ^ reg.(x) ^ "]"
        | Mem n -> "[" ^ string_of_int n ^ "]"
        | Const n -> string_of_int n
    in
    for i = 0 to Array.length gl - 1 do 
        let s =
        match gl.(i) with
            HLT -> "hlt"
            | INT n -> "int " ^ string_of_int n
            | MOV(a,b) -> "mov " ^ sod a ^ "," ^ sod b
            | INC a -> "inc " ^ sod a
            | DEC a -> "dec " ^ sod a
            | ADD(a,b) -> "add " ^ sod a ^ "," ^ sod b
            | SUB(a,b) -> "sub " ^ sod a ^ "," ^ sod b
            | MUL(a,b) -> "mul " ^ sod a ^ "," ^ sod b
            | DIV(a,b) -> "div " ^ sod a ^ "," ^ sod b
            | OR(a,b) -> "or " ^ sod a ^ "," ^ sod b
            | XOR(a,b) -> "xor " ^ sod a ^ "," ^ sod b
            | AND(a,b) -> "and " ^ sod a ^ "," ^ sod b
            | JLT(n,a,b) -> "jlt " ^ string_of_int n ^ "," ^ sod a ^ "," ^ sod b
            | JGT(n,a,b) -> "jgt " ^ string_of_int n ^ "," ^ sod a ^ "," ^ sod b
            | JEQ(n,a,b) -> "jeq " ^ string_of_int n ^ "," ^ sod a ^ "," ^ sod b
        in print_string s; print_newline ()
    done

let cleanup s =
    try
        let i = String.index s ';' in
        String.trim (String.sub s 0 i)
    with Not_found -> String.trim s

let get_ope s =
    let i = ref 0 in
    while !i < String.length s && s.[!i] <> ' ' do
        incr i
    done;
    String.uppercase (String.sub s 0 !i), 
        if !i = String.length s 
        then "" else String.sub s (!i+1) (String.length s - !i - 1)

let remove_space s =
    let r = Str.regexp " " in
    Str.global_replace r "" s

let reg_of_string s =
    if String.length s = 1
    then int_of_char (String.uppercase s).[0] - int_of_char 'A'
    else if (String.uppercase s) = "PC" then 8
    else failwith "Invalid register"

let get_arg s =
    let baddr = ref false in
    let sa = if s.[0] = '[' then ( baddr := true; String.sub s 1 (String.length s - 2))
        else s in
    let r = Str.regexp "^[0-9][0-9]*$" in
    if Str.string_match r sa 0
    then if !baddr then Mem (int_of_string sa) else Const (int_of_string sa)
    else if !baddr then IReg (reg_of_string sa) else Reg (reg_of_string sa)

let split_args s =
    let r = Str.regexp "," in
    Str.split r s

let get_instr ope l =
    match ope, l with
    | "MOV", [a;b] -> MOV(a,b)
    | "ADD", [a;b] -> ADD(a,b)
    | "SUB", [a;b] -> SUB(a,b)
    | "MUL", [a;b] -> MUL(a,b)
    | "DIV", [a;b] -> DIV(a,b)
    | "AND", [a;b] -> AND(a,b)
    | "OR", [a;b] -> OR(a,b)
    | "XOR", [a;b] -> XOR(a,b)
    | "INT", [Const a] -> INT a
    | "INC", [a] -> INC a
    | "DEC", [a] -> DEC a
    | "JLT", [Const n;a;b] -> JLT(n,a,b)
    | "JEQ", [Const n;a;b] -> JEQ(n,a,b)
    | "JGT", [Const n;a;b] -> JGT(n,a,b)
    | "HLT", [] -> HLT
    | _ -> failwith "Invalid instruction"

let read_ghc_from_file fn = 
    let f = open_in fn in
    let ghcl = ref [] in
    try
        while true do
            let s = cleanup (input_line f) in
            if s = "" then ()
            else begin
                let ope, args = get_ope s in
                let l = List.map get_arg (split_args (remove_space args)) in
                let i = get_instr ope l in
                ghcl := i :: !ghcl
            end
        done;
        failwith "Out of reach"
    with End_of_file -> Array.of_list (List.rev !ghcl)
