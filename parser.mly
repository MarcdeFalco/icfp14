%token <int> INT
%token <string> ID
%token UMINUS
%token PLUS MINUS TIMES DIV POW PIPE SQRT DOT HD TL
%token LPAREN RPAREN LBRACKET RBRACKET LACCO RACCO COMMA SEMICOLON COLON
%token FOR TO DO DONE LET EQUALS IF THEN ELSE REC FUN ARROW LEFTARROW IN
%token EOF RETURN VAR DOUBLEEQUALS GREATER LESS GREATEREQUALS LESSEQUALS
%token PRINT ATOM

%left DOUBLEEQUALS GREATER LESS GREATEREQUALS LESSEQUALS
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left POW DOT
%nonassoc UMINUS        /* highest precedence */

%{
open Ast

let rec chainize l =
    match l with
    | [] -> failwith "Invalid"
    | [x] -> x
    | t::q -> Chain(t, chainize(q))

let rec listize l =
    match l with
    | [] -> Const 0
    | t::q -> Cons(t, listize(q))

let rec consize l =
    match l with
    | [] -> failwith "Invalid"
    | [x] -> x
    | t::q -> Cons(t, consize(q))

let split_var_tuple vl e =
    let n = List.length vl in
    let rec aux l i =
        match l with
        [] -> []
        | t::q -> (t, DVar (Tuple(e,i,n)))::aux q (i+1)
    in aux vl 0

%}

%start <Ast.t> main
%%

main: t = body EOF { t }

body: d = decl r = main_expr { (d,r) }

decl:
    | LET vl = separated_nonempty_list(COMMA,ID) EQUALS e = main_expr IN q = decl { split_var_tuple vl e @ q }
    | LET vl = separated_nonempty_list(COMMA,ID) EQUALS e = main_expr { split_var_tuple vl e }
    | t = base_decl q = decl { t::q }
    | { [] }
    | error { raise (SyntaxError ("decl", $startpos, $endpos)) }

base_decl:
    | LET v = ID EQUALS e = main_expr IN { (v, DVar e) }
    | FUN f = ID LPAREN args = separated_list(COMMA, ID) RPAREN LACCO b = body RACCO  { (f, DFun(args,b)) }
    | error { raise (SyntaxError ("base_decl", $startpos, $endpos)) }

main_expr: 
    | l = separated_nonempty_list(SEMICOLON, expr) { chainize(l) }

(*expr : a = bexpr { SetFileInfo(a, $startpos.pos_cnum, $endpos.pos_cnum) } 

b*)expr: a = expr PLUS b = expr { Add(a,b) }
    | a = expr TIMES b = expr { Mul(a,b) }
    | a = expr DIV b = expr { Div(a,b) }
    | a = expr MINUS b = expr { Sub(a,b) }
    | a = expr DOUBLEEQUALS b = expr { Equals(a,b) }
    | a = expr GREATER b = expr { Greater(a,b) }
    | a = expr LESS b = expr { Greater(b,a) }
    | a = expr GREATEREQUALS b = expr { GreaterEquals(a,b) }
    | a = expr LESSEQUALS b = expr { GreaterEquals(b,a) }
    | a = expr LBRACKET b = INT PIPE c = INT RBRACKET { Tuple(a,b,c) }
    | a = expr LBRACKET b = INT RBRACKET { Tuple(a,b,2) }
    | a = expr DOT HD { Head a }
    | a = expr DOT TL { Tail a }
    | LPAREN RPAREN { Nop }
    | LBRACKET l = separated_list(SEMICOLON, expr) RBRACKET { listize(l) }
    | RETURN a = expr { Chain(Print a, a) }
    | PRINT a = expr { Print a }
    | v = ID LEFTARROW a = expr { Assign(v,a) }
    | ATOM LPAREN a = expr RPAREN { Atom a }
    | LPAREN e = main_expr RPAREN { e }
    | f = ID LPAREN args = separated_list(COMMA, expr) RPAREN { Call(f,args) }
    | LPAREN a = expr COMMA l = separated_nonempty_list(COMMA, expr) RPAREN { consize(a::l) }
    | c = INT { Const c }
    | v = ID { Var v }
    | IF cond = expr THEN bthen = expr ELSE belse = expr { If(cond,bthen,belse) }
    | error { raise (SyntaxError ("expr", $startpos, $endpos)) }
