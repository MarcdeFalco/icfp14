open Gcc

type address = Ret of int32 * frame | Join of int32 | Stop

type machine = {
    mutable pc : int32;
    data : data StackArray.t;
    control : address Stack.t;
    mutable frame : frame;
    mutable code : instr array
    }

let init_machine mac code =
    let rec fr = { ancestors = [||]; locals = [||]; dummy = true } in
    mac.frame <- fr;
    mac.code <- code;
    mac.pc <- Int32.zero;
    StackArray.clear mac.data;
    Stack.clear mac.control;
    Stack.push Stop mac.control

let dummy_machine () =
    let rec fr = { ancestors = [||]; locals = [||]; dummy = true } in
    { pc = Int32.zero; data = StackArray.create (Int Int32.zero); 
        control = Stack.create (); frame = fr; code = [||] }


let rec print_data d = match d with
    | Int n -> print_int (Int32.to_int n)
    | Cons (a,b) -> print_data a;
        print_string ",";
        print_data b
    | Closure (a,_) -> print_string "<";
        print_int (Int32.to_int a);
        print_string ",env>"

let print_address a = match a with
    | Ret (n,fp) -> print_string "R"; print_int (Int32.to_int n);
        print_string " "
    | Join n -> print_string "J"; print_int (Int32.to_int n);
        print_string " "
    | Stop -> print_string "Stop"

let rec print_frame f =
    print_string "[ ";
    Array.iter (fun d -> print_data d; print_string " ") f.locals;
    print_string "]\n";
    if f.ancestors != [||] then print_frame f.ancestors.(0)

let dump_machine mac =
    Printf.printf "PC : %d\n" (Int32.to_int mac.pc);
    print_string "Data stack : ";
    StackArray.iter print_data mac.data;
    print_newline ();
    print_string "Control stack : ";
    Stack.iter print_address mac.control;
    print_newline ();
    print_string "Frames : ";
    print_frame mac.frame;
    print_newline ()

let data_to_int d =
    match d with
    | Int n -> n
    | _ -> failwith "Invalid conversion of data to int"

let data_to_couple d =
    match d with
    | Cons(a,b) -> (data_to_int a, data_to_int b)
    | _ -> failwith "Invalid conversion of data to int"

let rec data_to_list f d =
    match d with
    | Int n when n = Int32.zero -> []
    | Cons(a,b) -> f a :: data_to_list f b

let get_main_frame_data mac =
    let rec aux f oldf =
        if f.ancestors != [||]
        then aux f.ancestors.(0) f
        else oldf.locals
    in aux mac.frame mac.frame

exception TagMismatch
let intofpop mac = 
    match StackArray.pop mac.data with
    Int x -> x | _ -> raise TagMismatch
let consofpop mac =
    match StackArray.pop mac.data with
    Cons (a,b) -> a, b | _ -> raise TagMismatch
let closofpop mac =
    match StackArray.pop mac.data with
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

let profiling = Hashtbl.create 42

(*
let calls = Hashtbl.create 42 

let register_call f tail =
    (*Printf.printf "%s " f;*)
    if not (Hashtbl.mem calls f)
    then  Hashtbl.add calls f (0,0);
    let cb, ct = Hashtbl.find calls f in
    Hashtbl.replace calls f (if tail then (cb,ct+1) else (cb+1,ct))
*)

let add mac =
    let a = intofpop mac in
    let b = intofpop mac in
    StackArray.push (Int (Int32.add a b)) mac.data;
    mac.pc <- Int32.succ mac.pc 

let sub mac =
    let a = intofpop mac in
    let b = intofpop mac in
    StackArray.push (Int (Int32.sub b a)) mac.data;
    mac.pc <- Int32.succ mac.pc

let load n i mac =
    let n = Int32.to_int n in
    let f = if n = 0 then mac.frame else mac.frame.ancestors.(n-1) in
    StackArray.push f.locals.(Int32.to_int i) mac.data;
    mac.pc <- Int32.succ mac.pc

let store n i mac =
    let n = Int32.to_int n in
    let f = if n = 0 then mac.frame else mac.frame.ancestors.(n-1) in
    f.locals.(Int32.to_int i) <- StackArray.pop mac.data;
    mac.pc <- Int32.succ mac.pc

let cons mac =
    let b = StackArray.pop mac.data in 
    let a = StackArray.pop mac.data in
    StackArray.push (Cons (a,b)) mac.data;
    mac.pc <- Int32.succ mac.pc

let car mac = 
    let a, b = consofpop mac in
    StackArray.push a mac.data;
    mac.pc <- Int32.succ mac.pc

let cdr mac =
    let a, b = consofpop mac in
    StackArray.push b mac.data;
    mac.pc <- Int32.succ mac.pc

let ap sf n mac =
    (*
    register_call sf false;
    *)
    let f, e = closofpop mac in
    let fp = { ancestors = Array.append [|e|] e.ancestors; locals = Array.create (Int32.to_int n)
        (Int Int32.zero); dummy = false } in
    for i = (Int32.to_int n)-1 downto 0 do
        fp.locals.(i) <- StackArray.pop mac.data
    done;
    Stack.push (Ret (Int32.succ mac.pc, mac.frame)) mac.control;
    mac.frame <- fp;
    mac.pc <- f

let register_instr i =
    let mnemo = Gcc.pp_mnemo i in
    if Hashtbl.mem profiling mnemo
    then Hashtbl.replace profiling mnemo (1 + Hashtbl.find profiling mnemo)
    else Hashtbl.add profiling mnemo 1

let eval mac =
    let instr = mac.code.(Int32.to_int mac.pc) in
    (* register_instr instr; *)
    (*
    dump_machine mac;
    Printf.printf "[%d] %s\n" (Int32.to_int mac.pc) (Gcc.pp_instr "" instr);
    *)
    match instr with
    | FILEINFO (a,b) -> currentPos := (a,b); mac.pc <- Int32.succ mac.pc
    | LDC n -> StackArray.push (Int n) mac.data; mac.pc <- Int32.succ mac.pc
    | LD (n, i) -> load n i mac
    | ADD -> add mac
    | SUB -> sub mac
    | MUL -> let a = intofpop mac in
        let b = intofpop mac in
        StackArray.push (Int (Int32.mul a b)) mac.data;
        mac.pc <- Int32.succ mac.pc
    | DIV -> let a = intofpop mac in
        let b = intofpop mac in
        StackArray.push (Int (Int32.div b a)) mac.data;
        mac.pc <- Int32.succ mac.pc
    | CEQ -> let a = intofpop mac in
        let b = intofpop mac in
        StackArray.push (Int (if a = b then Int32.one else Int32.zero)) mac.data;
        mac.pc <- Int32.succ mac.pc
    | CGT -> let a = intofpop mac in
        let b = intofpop mac in
        StackArray.push (Int (if a < b then Int32.one else Int32.zero)) mac.data;
        mac.pc <- Int32.succ mac.pc
    | CGTE -> let a = intofpop mac in
        let b = intofpop mac in
        StackArray.push (Int (if a <= b then Int32.one else Int32.zero)) mac.data;
        mac.pc <- Int32.succ mac.pc
    | ATOM -> let a = StackArray.pop mac.data in
        StackArray.push (Int (match a with Int _ -> Int32.one | _ -> Int32.zero)) mac.data;
        mac.pc <- Int32.succ mac.pc
    | CONS -> cons mac
    | CAR -> car mac
    | CDR ->  cdr mac
    | SEL(t,f) -> let x = intofpop mac in
        Stack.push (Join (Int32.succ mac.pc)) mac.control;
        mac.pc <- if x = Int32.zero then f else t
    | JOIN -> let x = joinofpop mac in
        mac.pc <- x
    | LDF f -> let c = Closure (f, mac.frame) in
        StackArray.push c mac.data;
        mac.pc <- Int32.succ mac.pc
    | AP (sf,n) -> ap sf n mac
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
            let fp = { ancestors = Array.append [|mac.frame|] mac.frame.ancestors; locals = Array.create (Int32.to_int n)
            (Int Int32.zero); dummy = true } in
        mac.frame <- fp;
        mac.pc <- Int32.succ mac.pc
    | RAP n ->
        let f, fp = closofpop mac in
        if not fp.dummy then raise FrameMismatch;
        if Array.length fp.locals != Int32.to_int n then raise FrameMismatch;
        for i = Int32.to_int n-1 downto 0 do
            fp.locals.(i) <- StackArray.pop mac.data
        done;
        Stack.push (Ret (Int32.succ mac.pc,fp.ancestors.(0))) mac.control;
        mac.frame <- fp;
        mac.pc <- f
    | TSEL(t,f) -> let x = intofpop mac in
        mac.pc <- if x = Int32.zero then f else t
    | TAP (sf,n) -> 
        (*
        register_call sf true;
        *)
        let f, e = closofpop mac in
        let fp = { ancestors = Array.append [|e|] e.ancestors; locals = Array.create (Int32.to_int n)
                (Int Int32.zero); dummy = false } in
        for i = Int32.to_int n-1 downto 0 do
            fp.locals.(i) <- StackArray.pop mac.data
        done;
        mac.frame <- fp;
        mac.pc <- f
    | TRAP n ->
        let f, fp = closofpop mac in
        if not fp.dummy then raise FrameMismatch;
        if Array.length fp.locals != Int32.to_int n then raise FrameMismatch;
        for i = Int32.to_int n-1 downto 0 do
            fp.locals.(i) <- StackArray.pop mac.data
        done;
        mac.frame <- fp;
        mac.pc <- f
    | ST (n, i) -> store n i mac
    | STOP -> raise MachineStop
    | DBUG -> (* print_data (Stack.pop mac.data); print_newline (); flush stdout;*)
            mac.pc <- Int32.succ mac.pc
    | BRK -> mac.pc <- Int32.succ mac.pc
    | _ -> raise UnhandledInstruction

exception CycleExceeded

exception GccRun of string * (int * int)

let run ?verbose:(bverb=false) bmain mac =
    let cycle = ref 0 in
    let t = Sys.time () in
    try
        (* Hashtbl.clear calls; *)
        while (bmain && Sys.time() < t +. 60.0)
            || (not bmain && !cycle < 3072000) do
            incr cycle;
            eval mac;
            if bverb
            then begin
                Printf.printf "Instruction %d\n" !cycle;
                dump_machine mac
            end
        done;
        (*
        Hashtbl.iter 
            (fun  f c -> let base, tail = c in Printf.printf "%s : %d + %d\n" f base tail)
            calls;
        *)
        raise CycleExceeded
    with MachineStop -> !cycle
    (*
    | e -> let se = Printexc.to_string e in raise (GccRun(se, !currentPos))
    *)

let dummy_for_typing_profiling () = Hashtbl.add profiling "toto" 0

let main mac world codes =
    let rec fp = { ancestors = [||]; locals = Array.create 2 (Int Int32.zero); dummy = false } in
    fp.locals.(0) <- world;
    fp.locals.(1) <- codes;
    mac.frame <- fp;
    mac.pc <- Int32.zero;
    run true mac;
    (*dump_machine mac;*)
    let state, step_closure = consofpop mac in
    (state, step_closure)

let step mac state world step_closure =
    let Closure(f,e) = step_closure in
    let fp = { ancestors = Array.append [|e|] e.ancestors; locals = Array.create 2 (Int Int32.zero); dummy = false } in
    fp.locals.(0) <- state;
    fp.locals.(1) <- world;
    Stack.push Stop mac.control;
    mac.frame <- fp;
    mac.pc <- f;
    let cycle = run false mac in
    (*dump_machine mac;*)
    let state, Int dir = consofpop mac in
    (state, Common.dir_of_int (Int32.to_int dir), cycle)
