open Ghc
open Common

type machine = {
    regs : int array;
    mutable pc : int;
    data : int array;
    code : instr array
    }

type ghost_vitality = Standard | Fright | Invisible

let int_of_vit v = 
    match v with
    Standard -> 0 | Fright -> 1 | _ -> 2

type ghost = {
    mutable vit : ghost_vitality;
    mutable pos : Common.pos;
    mutable dir : Common.direction;
    mac : machine
    }

let init_machine code = { regs = Array.make 8 0; pc = 0; data = Array.make 256 0; code = code }

let init_ghost pos code = { vit = Standard; pos = pos; dir = DOWN; mac = init_machine code }

exception Found of int
let index a v =
try
    for i = 0 to Array.length a - 1 do
        if a.(i) = v then raise (Found i)
    done;
    raise Not_found
with Found n -> n

exception EvalError
exception Stop

let eval target_dir ghost map ghosts lambdamanpos =
    let mac = ghost.mac in

    let rec get a =
        match a with
        | Reg n -> if n < 8 then mac.regs.(n) else mac.pc
        | IReg n -> mac.data.(get (Reg n))
        | Mem n -> mac.data.(n)
        | Const n -> n
    in

    let cleanup v = 
        let sv = ref v in
        while !sv < 0 do
            sv := !sv + 256
        done;
        sv := !sv mod 256;
        !sv
    in

    let set a v =
        let v = cleanup v in
        match a with
        | Reg n -> if n < 8 then mac.regs.(n) <- v
                    else mac.pc <- v
        | IReg n -> mac.data.(get (Reg n)) <- v
        | Mem n -> mac.data.(n) <- v
        | Const n -> raise EvalError
    in

    let instr = mac.code.(mac.pc) in
    match instr with
    | MOV(a,b) -> set a (get b)
    | INC a -> set a (get a + 1) 
    | DEC a -> set a (get a - 1) 
    | ADD(a,b) -> set a (get a + get b) 
    | SUB(a,b) -> set a (get a - get b) 
    | MUL(a,b) -> set a (get a * get b)
    | DIV(a,b) -> set a (get a / get b)
    | AND(a,b) -> set a (get a land get b)
    | OR(a,b) -> set a (get a lor get b)
    | XOR(a,b) -> set a (get a lxor get b)
    | JLT(n,a,b) -> if get a < get b then mac.pc <- n
    | JEQ(n,a,b) -> if get a = get b then mac.pc <- n
    | JGT(n,a,b) -> if get a > get b then mac.pc <- n
    | INT n -> begin
        match n with
        | 0 -> 
            let d = dir_of_int mac.regs.(0) in
            target_dir := d
        | 1 -> mac.regs.(0) <- lambdamanpos.x;
             mac.regs.(1) <- lambdamanpos.y
        | 2 -> raise EvalError
        | 3 -> mac.regs.(0) <- index ghosts ghost
        | 4 -> let p = Common.map_get_starting_ghost_pos map mac.regs.(0) in
            mac.regs.(0) <- p.x;
            mac.regs.(1) <- p.y
        | 5 -> let p = ghosts.(mac.regs.(0)).pos in
            mac.regs.(0) <- p.x;
            mac.regs.(1) <- p.y
        | 6 -> let g = ghosts.(mac.regs.(0)) in
            mac.regs.(0) <- int_of_vit g.vit;
            mac.regs.(1) <- Common.int_of_dir g.dir
        | 7 -> mac.regs.(0) <- Common.int_of_cell map.(mac.regs.(1)).(mac.regs.(0))
        | 8 -> Printf.printf "trace ghost%d: %d %d %d %d %d %d %d %d %d\n"
            (index ghosts ghost)
            mac.pc mac.regs.(0)
            mac.regs.(1) mac.regs.(2)
            mac.regs.(3)
            mac.regs.(4)
            mac.regs.(5)
            mac.regs.(6)
            mac.regs.(7);
            flush stdout
    end
    | HLT -> raise Stop

let run ghost map ghosts lambdamanpos =
    let mac = ghost.mac in
    let target_dir = ref ghost.dir in
    try
        let instr_count = ref 0 in
        mac.pc <- 0;
        while true do
            let oldpc = mac.pc in
            eval target_dir ghost map ghosts lambdamanpos;
            (*
            Printf.printf "auto ghost%d: %d %d %d %d %d %d %d %d %d\n"
            (index ghosts ghost)
            mac.pc mac.regs.(0)
            mac.regs.(1) mac.regs.(2)
            mac.regs.(3)
            mac.regs.(4)
            mac.regs.(5)
            mac.regs.(6)
            mac.regs.(7);
            flush stdout;
            *)
            if oldpc = mac.pc
            then mac.pc <- mac.pc + 1;
            incr instr_count;
            if !instr_count = 1024
            then raise Stop
        done;
        failwith "Out of reach"
    with Stop -> !target_dir

