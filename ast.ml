exception SyntaxError of string * Lexing.position * Lexing.position

type expr =
    | Add of expr * expr
    | Sub of expr * expr
    | Mul of expr * expr
    | Div of expr * expr
    | Cons of expr * expr
    | Call of string * expr list
    | Const of int
    | Var of string
    | If of expr * expr * expr
    | Equals of expr * expr
    | Greater of expr * expr
    | GreaterEquals of expr * expr
    | Tuple of expr * int * int
    | Head of expr
    | Tail of expr
    | Chain of expr * expr
    | Print of expr
    | Atom of expr

type decl =
    | DVar of expr
    | DFun of string list * t
    | DDummy
and t = (string * decl) list * expr

let rec pp_expr e =
    match e with
    | Add(a,b) -> pp_expr a ^ "+" ^ pp_expr b
    | Cons(a,b) -> "(" ^ pp_expr a ^ "," ^ pp_expr b ^ ")"
    | Call(f,el) -> f ^ "(" ^ String.concat "," (List.map pp_expr el) ^ ")"
    | Const n -> string_of_int n
    | Var v -> v

let rec pp_ast (d,r) =
    String.concat "\n" 
        (List.map pp_decl d)
    ^ "\n" ^ pp_expr r
and pp_decl (s,d) =
    match d with
    DVar e -> "var " ^ s ^ " = " ^ pp_expr e ^ "\n"
    | DFun(al,b) -> "fun " ^ s ^ "(" ^ 
            String.concat "," al ^ ") {\n" ^ pp_ast b ^ "\n}\n"
    | DDummy -> "dummy" (* internal use only *)
