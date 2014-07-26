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
    | LDFs of string | LDF of int | AP of int
    | RTN
    | DUM of int | RAP of int
    | DBUG | STOP
    | TAP of int | TRAP of int

let pp_instr i =
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
    | AP n -> "AP " ^ string_of_int n
    | TAP n -> "TAP " ^ string_of_int n
    | RAP n -> "RAP " ^ string_of_int n
    | TRAP n -> "TRAP " ^ string_of_int n
    | LDFs s -> "LDF " ^ s
    | LDF s -> "LDF " ^ string_of_int s
    | RTN -> "RTN"
    | DUM n -> "DUM " ^ string_of_int n
    | DBUG -> "DBUG"
    | STOP -> "STOP"

let pp_code code = String.concat "\n" (List.map pp_instr code)

let absolute code =
    let rec aux code acc pos = 
        match code with 
        | [] -> acc
        | Label s::q -> aux q ((s,pos)::acc) pos
        | _::q -> aux q acc (pos+1)
    in
    let labels = aux code [] 0 in

    let rec replace code = 
        match code with
        | [] -> []
        | Label s :: q -> replace q
        | i :: q -> begin
            match i with
            | LDFs s -> LDF (List.assoc s labels)
            | SELs (a,b) -> SEL (List.assoc a labels, List.assoc b labels)
            | TSELs (a,b) -> TSEL (List.assoc a labels, List.assoc b labels)
            | _ -> i
        end :: replace q

    in Array.of_list (replace code)
