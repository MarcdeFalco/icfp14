type instr = 
    | Label of string
    | LDC of int32
    | LD of int32 * int32
    | ST of int32 * int32
    | ADD | SUB | MUL | DIV
    | CEQ | CGT | CGTE
    | ATOM
    | CONS | CAR | CDR
    | SELs of string * string 
    | SEL of int32 * int32
    | TSELs of string * string 
    | TSEL of int32 * int32 
    | JOIN
    | LDFs of string | LDF of int32 | AP of string * int32
    | RTN
    | DUM of int32 | RAP of int32
    | DBUG | STOP | BRK
    | TAP of string * int32 | TRAP of int32
    | FILEINFO of int * int

let pp_mnemo i =
    match i with
    | Label s -> failwith "No mnemo"
    | LDC n -> "LDC"
    | LD (a,b) -> "LD"
    | ST (a,b) -> "ST"
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
    | TSELs (a,b) -> "TSEL"
    | SELs (a,b) -> "SEL"
    | TSEL (a,b) -> "TSEL"
    | SEL (a,b) -> "SEL"
    | JOIN -> "JOIN"
    | AP (_,n) -> "AP"
    | TAP (_,n) -> "TAP"
    | RAP n -> "RAP"
    | TRAP n -> "TRAP"
    | LDFs s -> "LDF"
    | LDF s -> "LDF"
    | RTN -> "RTN"
    | DUM n -> "DUM"
    | DBUG -> "DBUG"
    | STOP -> "STOP"
    | FILEINFO (a,b) -> failwith "No mnemo"

let pp_instr src i =
    match i with
    | Label s -> s ^ ":"
    | LDC n -> "LDC " ^ Int32.to_string n
    | LD (a,b) -> "LD " ^ Int32.to_string a ^ " " ^ Int32.to_string b
    | ST (a,b) -> "ST " ^ Int32.to_string a ^ " " ^ Int32.to_string b
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
    | TSEL (a,b) -> "TSEL " ^ Int32.to_string a ^ " " ^ Int32.to_string b
    | SEL (a,b) -> "SEL " ^ Int32.to_string a ^ " " ^ Int32.to_string b
    | JOIN -> "JOIN"
    | AP (_,n) -> "AP " ^ Int32.to_string n
    | TAP (_,n) -> "TAP " ^ Int32.to_string n
    | RAP n -> "RAP " ^ Int32.to_string n
    | TRAP n -> "TRAP " ^ Int32.to_string n
    | LDFs s -> "LDF " ^ s
    | LDF s -> "LDF " ^ Int32.to_string s
    | RTN -> "RTN"
    | DUM n -> "DUM " ^ Int32.to_string n
    | DBUG -> "DBUG"
    | STOP -> "STOP"
    | FILEINFO (a,b) -> "; " ^ String.sub src a (b-a)

let absolute code =
    let rec aux code acc pos = 
        match code with 
        | [] -> acc
        | Label s::q -> aux q ((s,Int32.of_int pos)::acc) pos
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
    let r = Str.regexp "[ \t]+" in
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
    | "AP", [n] -> AP (Int32.to_string n, n)
    | "TAP", [n] -> TAP (Int32.to_string n, n)
    | "RAP", [n] -> RAP n
    | "TRAP", [n] -> TRAP n
    | "DUM", [n] -> DUM n
    | "BRK", [] -> BRK
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
                try
                    let i = get_instr (List.hd elems) (List.map Int32.of_string (List.tl elems)) in
                    gccl := i :: !gccl
                with e -> raise e
            end
        done;
        failwith "Out of reach"
    with End_of_file -> Array.of_list (List.rev !gccl)


type data = Int of int32 | Cons of data * data | Closure of int32 * frame
and frame = { ancestors : frame array; locals : data array; mutable dummy : bool }
