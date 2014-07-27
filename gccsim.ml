type data = Int of int | Cons of data * data | Closure of int * frame
and frame = { parent : frame; locals : data array; mutable dummy : bool }

type address = Ret of int * frame | Join of int | Stop

type machine = {
    mutable pc : int;
    data : data Stack.t;
    control : address Stack.t;
    mutable frame : frame;
    mutable code : Gcc.instr array
    }

let init_machine mac code =
    let rec fr = { parent = fr; locals = [||]; dummy = true } in
    mac.frame <- fr;
    mac.code <- code;
    mac.pc <- 0;
    Stack.clear mac.data;
    Stack.clear mac.control;
    Stack.push Stop mac.control

let dummy_machine () =
    let rec fr = { parent = fr; locals = [||]; dummy = true } in
    { pc = 0; data = Stack.create (); 
        control = Stack.create (); frame = fr; code = [||] }


let rec print_data d = match d with
    | Int n -> print_int n
    | Cons (a,b) -> print_data a;
        print_string ",";
        print_data b
    | Closure (a,_) -> print_string "<";
        print_int a;
        print_string ",env>"

let print_address a = match a with
    | Ret (n,fp) -> print_string "R"; print_int n;
        print_string " "
    | Join n -> print_string "J"; print_int n;
        print_string " "
    | Stop -> print_string "Stop"

let rec print_frame f =
    print_string "[ ";
    Array.iter (fun d -> print_data d; print_string " ") f.locals;
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
exception UnhandledInstruction

let currentPos = ref (0,0)

let calls = Hashtbl.create 42 

let register_call f tail =
    (*Printf.printf "%s " f;*)
    if not (Hashtbl.mem calls f)
    then  Hashtbl.add calls f (0,0);
    let cb, ct = Hashtbl.find calls f in
    Hashtbl.replace calls f (if tail then (cb,ct+1) else (cb+1,ct))

let eval mac =
    let instr = mac.code.(mac.pc) in
    (*
    dump_machine mac;
    Printf.printf "[%d] %s\n" mac.pc (Gcc.pp_instr instr);
    *)
    match instr with
    | FILEINFO (a,b) -> currentPos := (a,b); mac.pc <- mac.pc + 1
    | LDC n -> Stack.push (Int n) mac.data; mac.pc <- mac.pc + 1
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
        let b = Stack.pop mac.data in 
        let a = Stack.pop mac.data in
        Stack.push (Cons (a,b)) mac.data;
        mac.pc <- mac.pc + 1
    | CAR -> let a, b = consofpop mac in
        Stack.push a mac.data;
        mac.pc <- mac.pc + 1
    | CDR -> let a, b = consofpop mac in
        Stack.push b mac.data;
        mac.pc <- mac.pc + 1
    | SEL(t,f) -> let x = intofpop mac in
        Stack.push (Join (mac.pc+1)) mac.control;
        mac.pc <- if x = 0 then f else t
    | JOIN -> let x = joinofpop mac in
        mac.pc <- x
    | LDF f -> let c = Closure (f, mac.frame) in
        Stack.push c mac.data;
        mac.pc <- mac.pc + 1
    | AP (sf,n) -> 
        register_call sf false;
        let f, e = closofpop mac in
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
    | TSEL(t,f) -> let x = intofpop mac in
        mac.pc <- if x = 0 then f else t
    | TAP (sf,n) -> 
        register_call sf true;
        let f, e = closofpop mac in
        let fp = { parent = e; locals = Array.create n (Int 42); dummy = false } in
        for i = n-1 downto 0 do
            fp.locals.(i) <- Stack.pop mac.data
        done;
        mac.frame <- fp;
        mac.pc <- f
    | TRAP n ->
        let f, fp = closofpop mac in
        if not fp.dummy then raise FrameMismatch;
        if Array.length fp.locals != n then raise FrameMismatch;
        for i = n-1 downto 0 do
            fp.locals.(i) <- Stack.pop mac.data
        done;
        mac.frame <- fp;
        mac.pc <- f
    | ST (n, i) ->
        let frame = ref mac.frame in
        let rn = ref n in
        while !rn > 0 do
            frame := !frame.parent;
            rn := !rn - 1
        done;
        !frame.locals.(i) <- Stack.pop mac.data;
        mac.pc <- mac.pc + 1
    | STOP -> raise MachineStop
    | DBUG -> print_data (Stack.pop mac.data); print_newline (); flush stdout;
            mac.pc <- mac.pc+1
    | _ -> raise UnhandledInstruction

exception CycleExceeded

exception GccRun of string * (int * int)
let run mac =
    let cycle = ref 0 in
    try
        Hashtbl.clear calls;
        while !cycle < 3072000 do
            incr cycle; eval mac
        done;
        Hashtbl.iter 
            (fun  f c -> let base, tail = c in Printf.printf "%s : %d + %d\n" f base tail)
            calls;
        raise CycleExceeded
    with MachineStop -> !cycle
         | e -> let se = Printexc.to_string e in raise (GccRun(se, !currentPos))

let main mac world codes =
    let rec fp = { parent = fp; locals = Array.create 2 (Int 42); dummy = false } in
    fp.locals.(0) <- world;
    fp.locals.(1) <- codes;
    mac.frame <- fp;
    mac.pc <- 0;
    run mac;
    (*dump_machine mac;*)
    let state, step_closure = consofpop mac in
    (state, step_closure)

let step mac state world step_closure =
    let Closure(f,e) = step_closure in
    let fp = { parent = e; locals = Array.create 2 (Int 42); dummy = false } in
    fp.locals.(0) <- state;
    fp.locals.(1) <- world;
    Stack.push Stop mac.control;
    mac.frame <- fp;
    mac.pc <- f;
    let cycle = run mac in
    (*dump_machine mac;*)
    let state, Int dir = consofpop mac in
    (state, Common.dir_of_int dir, cycle)
