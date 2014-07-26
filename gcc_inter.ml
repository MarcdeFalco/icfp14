type data = Int of int | Cons of data * data | Closure of int * frame
and frame = { parent : frame; locals : data array; mutable dummy : bool }

type address = Ret of int * frame | Join of int | Stop

type machine = {
    mutable pc : int;
    data : data Stack.t;
    control : address Stack.t;
    mutable frame : frame
    }

let init_machine () =
    let rec fr = { parent = fr; locals = [||]; dummy = true } in
    let mac = { pc = 0; data = Stack.create (); 
    control = Stack.create (); frame = fr } in
    Stack.push Stop mac.control;
    mac

let rec print_data d = match d with
    | Int n -> print_int n; print_string " "
    | Cons (a,b) -> print_string "(";
        print_data a;
        print_string ",";
        print_data b;
        print_string ") "
    | Closure (a,_) -> print_string "<";
        print_int a;
        print_string ",env> "

let print_address a = match a with
    | Ret (n,fp) -> print_string "R"; print_int n;
        print_string " "
    | Join n -> print_string "J"; print_int n;
        print_string " "
    | Stop -> print_string "Stop"

let rec print_frame f =
    print_string "[ ";
    Array.iter print_data f.locals;
    print_string "]\n";
    if f.parent != f then print_frame f.parent

let dump_machine mac =
    Printf.printf "PC : %d\n" mac.pc;
    print_string "Data stack : ";
    Stack.iter print_data mac.data;
    print_newline ();
    print_string "Control stack : ";
    Stack.iter print_address mac.control;
    print_newline ();
    print_string "Frames : ";
    print_frame mac.frame;
    print_newline ()

type instr = 
    | LDC of int 
    | LD of int * int
    | ADD | SUB | MUL | DIV
    | CEQ | CGT | CGTE
    | ATOM
    | CONS | CAR | CDR
    | SEL of int * int | JOIN
    | LDF of int | AP of int
    | RTN
    | DUM of int | RAP of int
    | STOP

exception TagMismatch
let intofpop mac = 
    match Stack.pop mac.data with
    Int x -> x | _ -> raise TagMismatch
let consofpop mac =
    match Stack.pop mac.data with
    Cons (a,b) -> a, b | _ -> raise TagMismatch
let closofpop mac =
    match Stack.pop mac.data with
    Closure (f,e) -> f, e | _ -> raise TagMismatch

exception ControlMismatch
let joinofpop mac =
    match Stack.pop mac.control with
    Join x -> x | _ -> raise ControlMismatch
let retofpop mac =
    match Stack.pop mac.control with
    Ret (x,f) -> x,f | _ -> raise ControlMismatch

exception FrameMismatch
exception MachineStop

let eval mac instr =
    match instr with
    | LDC n -> 
            Stack.push (Int n) mac.data; mac.pc <- mac.pc + 1
    | LD (n, i) ->
        let frame = ref mac.frame in
        let rn = ref n in
        while !rn > 0 do
            frame := !frame.parent;
            rn := !rn - 1
        done;
        Stack.push !frame.locals.(i) mac.data;
        mac.pc <- mac.pc + 1
    | ADD -> let a = intofpop mac in
        let b = intofpop mac in
        Stack.push (Int (a+b)) mac.data;
        mac.pc <- mac.pc + 1
    | SUB -> let a = intofpop mac in
        let b = intofpop mac in
        Stack.push (Int (b-a)) mac.data;
        mac.pc <- mac.pc + 1
    | MUL -> let a = intofpop mac in
        let b = intofpop mac in
        Stack.push (Int (a*b)) mac.data;
        mac.pc <- mac.pc + 1
    | DIV -> let a = intofpop mac in
        let b = intofpop mac in
        Stack.push (Int (b/a)) mac.data;
        mac.pc <- mac.pc + 1
    | CEQ -> let a = intofpop mac in
        let b = intofpop mac in
        Stack.push (Int (if a = b then 1 else 0)) mac.data;
        mac.pc <- mac.pc + 1
    | CGT -> let a = intofpop mac in
        let b = intofpop mac in
        Stack.push (Int (if a < b then 1 else 0)) mac.data;
        mac.pc <- mac.pc + 1
    | CGTE -> let a = intofpop mac in
        let b = intofpop mac in
        Stack.push (Int (if a <= b then 1 else 0)) mac.data;
        mac.pc <- mac.pc + 1
    | ATOM -> let a = Stack.pop mac.data in
        Stack.push (Int (match a with Int _ -> 1 | _ -> 0)) mac.data;
        mac.pc <- mac.pc + 1
    | CONS -> 
        let a = Stack.pop mac.data in
        let b = Stack.pop mac.data in 
        Stack.push (Cons (a,b)) mac.data;
        mac.pc <- mac.pc + 1
    | CAR -> let a, b = consofpop mac in
        Stack.push a mac.data;
        mac.pc <- mac.pc + 1
    | CDR -> let a, b = consofpop mac in
        Stack.push b mac.data;
        mac.pc <- mac.pc + 1
    | SEL(f,t) -> let x = intofpop mac in
        Stack.push (Join (mac.pc+1)) mac.control;
        mac.pc <- if x = 0 then f else t
    | JOIN -> let x = joinofpop mac in
        mac.pc <- x
    | LDF f -> let c = Closure (f, mac.frame) in
        Stack.push c mac.data;
        mac.pc <- mac.pc + 1
    | AP n -> let f, e = closofpop mac in
        let fp = { parent = e; locals = Array.create n (Int 42); dummy = false } in
        for i = n-1 downto 0 do
            fp.locals.(i) <- Stack.pop mac.data
        done;
        Stack.push (Ret (mac.pc+1, mac.frame)) mac.control;
        mac.frame <- fp;
        mac.pc <- f
    | RTN -> begin
        let x = Stack.pop mac.control in
        match x with
        | Stop -> raise MachineStop
        | Ret (x,f) -> 
            mac.frame <- f;
            mac.pc <- x
        | _ -> raise ControlMismatch
        end
    | DUM n ->
        let fp = { parent = mac.frame; locals = Array.create n (Int 42); dummy = true } in
        mac.frame <- fp;
        mac.pc <- mac.pc + 1
    | RAP n ->
        let f, fp = closofpop mac in
        if not fp.dummy then raise FrameMismatch;
        if Array.length fp.locals != n then raise FrameMismatch;
        for i = n-1 downto 0 do
            fp.locals.(i) <- Stack.pop mac.data
        done;
        Stack.push (Ret (mac.pc+1,fp.parent)) mac.control;
        mac.frame <- fp;
        mac.pc <- f
    | STOP -> raise MachineStop

let main =
    Printexc.record_backtrace true;
    try
        (* let code = [| LDC 21; LDF 4; AP 1; RTN; LD (0,0); LD (0,0); ADD; RTN |] in *)
        let code = [| DUM 2; LDF 16; LDF 10; LDF 6; RAP 2; RTN;
                LDC 1; LD (0,0); AP 1; RTN;
                LD (0,0); LDC 1; SUB; LD (1,0); AP 1; RTN;
                LD (0,0); LDC 1; ADD; LD (1,1); AP 1; RTN |] in
        let mac = init_machine () in
        for i = 0 to 20 do
            let instr = code.(mac.pc) in
            eval mac instr;
            dump_machine mac
        done
    with e ->
        Printf.printf "Exception %s raised at %d:\n" (Printexc.to_string e);
        Printexc.print_backtrace stdout
