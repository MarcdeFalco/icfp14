type instr = 
    | Label of string
    | LDC of int 
    | LD of int * int
    | ADD | SUB | MUL | DIV
    | CEQ | CGT | CGTE
    | ATOM
    | CONS | CAR | CDR
    | SEL of string * string | JOIN
    | LDF of string | AP of int
    | RTN
    | DUM of int | RAP of int
    | DBUG | STOP

let pp_instr i =
    match i with
    | Label s -> s ^ ":"
    | LDC n -> "LDC " ^ string_of_int n
    | LD (a,b) -> "LD " ^ string_of_int a ^ " " ^ string_of_int b
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
    | SEL (a,b) -> "SEL " ^ a ^ " " ^ b
    | JOIN -> "JOIN"
    | AP n -> "AP " ^ string_of_int n
    | RAP n -> "RAP " ^ string_of_int n
    | LDF s -> "LDF " ^ s
    | RTN -> "RTN"
    | DUM n -> "DUM " ^ string_of_int n
    | DBUG -> "DBUG"
    | STOP -> "STOP"

let pp_code code = String.concat "\n" (List.map pp_instr code)

