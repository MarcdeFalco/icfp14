type instr = 
    | Label of string
    | LDC of int 
    | LD of int * int
    | ST of int * int
    | ADD | SUB | MUL | DIV
    | CEQ | CGT | CGTE
    | ATOM
    | CONS | CAR | CDR
    | SELs of string * string 
    | SEL of int * int 
    | TSELs of string * string 
    | TSEL of int * int 
    | JOIN
    | LDFs of string | LDF of int | AP of string * int
    | RTN
    | DUM of int | RAP of int
    | DBUG | STOP
    | TAP of string * int | TRAP of int
    | FILEINFO of int * int

let pp_instr src i =
    match i with
    | Label s -> s ^ ":"
    | LDC n -> "LDC " ^ string_of_int n
    | LD (a,b) -> "LD " ^ string_of_int a ^ " " ^ string_of_int b
    | ST (a,b) -> "ST " ^ string_of_int a ^ " " ^ string_of_int b
    | ADD -> "ADD"
    | SUB -> "SUB"
    | MUL -> "MUL"
    | DIV -> "DIV"
    | CEQ -> "CEQ"
    | CGT -> "CGT"
    | CGTE -> "CGTE"
    | ATOM -> "ATOM"
    | CONS -> "CONS"
    | CAR -> "CAR"
    | CDR -> "CDR"
    | TSELs (a,b) -> "TSEL " ^ a ^ " " ^ b
    | SELs (a,b) -> "SEL " ^ a ^ " " ^ b
    | TSEL (a,b) -> "TSEL " ^ string_of_int a ^ " " ^ string_of_int b
    | SEL (a,b) -> "SEL " ^ string_of_int a ^ " " ^ string_of_int b
    | JOIN -> "JOIN"
    | AP (_,n) -> "AP " ^ string_of_int n
    | TAP (_,n) -> "TAP " ^ string_of_int n
    | RAP n -> "RAP " ^ string_of_int n
    | TRAP n -> "TRAP " ^ string_of_int n
    | LDFs s -> "LDF " ^ s
    | LDF s -> "LDF " ^ string_of_int s
    | RTN -> "RTN"
    | DUM n -> "DUM " ^ string_of_int n
    | DBUG -> "DBUG"
    | STOP -> "STOP"
    | FILEINFO (a,b) -> "; " ^ String.sub src a (b-a)

let absolute code =
    let rec aux code acc pos = 
        match code with 
        | [] -> acc
        | Label s::q -> aux q ((s,pos)::acc) pos
        (*| FILEINFO(_,_)::q -> aux q acc pos*)
        | _::q -> aux q acc (pos+1)
    in
    let labels = aux code [] 0 in

    let rec replace c code = 
        match code with
        | [] -> []
        | Label s :: q -> replace c q
        | i :: q -> begin
            match i with
            | LDFs s -> LDF (List.assoc s labels)
            | SELs (a,b) -> SEL (List.assoc a labels, List.assoc b labels)
            | TSELs (a,b) -> TSEL (List.assoc a labels, List.assoc b labels)
            | _ -> i
        end :: replace (c+1) q

    in Array.of_list (replace 0 code)

let cleanup s =
    try
        let i = String.index s ';' in
        String.trim (String.sub s 0 i)
    with Not_found -> String.trim s

let split_line s =
    let r = Str.regexp " +" in
    Str.split r s

let get_instr ope l =
    match ope, l with
    | "LDC", [n] -> LDC n
    | "LDF", [n] -> LDF n
    | "LD", [a;b] -> LD (a,b)
    | "ST", [a;b] -> ST (a,b)
    | "ADD", [] -> ADD
    | "SUB", [] -> SUB
    | "MUL", [] -> MUL
    | "DIV", [] -> DIV
    | "CEQ", [] -> CEQ
    | "CGT", [] -> CGT
    | "CGTE", [] -> CGTE
    | "ATOM", [] -> ATOM
    | "CONS", [] -> CONS
    | "CAR", [] -> CAR
    | "CDR", [] -> CDR
    | "JOIN", [] -> JOIN
    | "RTN", [] -> RTN
    | "DBUG", [] -> DBUG
    | "STOP", [] -> STOP
    | "SEL", [a;b] -> SEL (a,b)
    | "TSEL", [a;b] -> TSEL (a,b)
    | "AP", [n] -> AP (string_of_int n, n)
    | "TAP", [n] -> TAP (string_of_int n, n)
    | "RAP", [n] -> RAP n
    | "TRAP", [n] -> TRAP n
    | "DUM", [n] -> DUM n
    | s,l -> failwith (Printf.sprintf "Invalid instruction: %s" s)

let read_gcc_from_file fn = 
    let f = open_in fn in
    let gccl = ref [] in
    try
        while true do
            let s = cleanup (input_line f) in
            if s = "" then ()
            else begin
                let elems = split_line s in
                let i = get_instr (List.hd elems) (List.map int_of_string (List.tl elems)) in
                gccl := i :: !gccl
            end
        done;
        failwith "Out of reach"
    with End_of_file -> Array.of_list (List.rev !gccl)
