%token <int> INT
%token <string> ID
%token UMINUS
%token PLUS MINUS TIMES DIV POW PIPE SQRT DOT HD TL
%token LPAREN RPAREN LBRACKET RBRACKET LACCO RACCO COMMA SEMICOLON COLON
%token FOR TO DO DONE LET EQUALS IF THEN ELSE REC FUN ARROW LEFTARROW IN
%token EOF RETURN VAR DOUBLEEQUALS GREATER LESS GREATEREQUALS LESSEQUALS
%token PRINT

%left DOUBLEEQUALS GREATER LESS GREATEREQUALS LESSEQUALS
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left POW DOT
%nonassoc UMINUS        /* highest precedence */

%{
open Ast


%}

%start <Ast.t> main
%%

main: t = body EOF { t }

body: d = list(decl) r = expr { (d,r) }

decl:
    | VAR v = ID EQUALS e = expr { (v, DVar e) }
    | FUN f = ID LPAREN args = separated_list(COMMA, ID) RPAREN LACCO b = body RACCO  { (f, DFun(args,b)) }
    | error { raise (SyntaxError ("decl", $startpos, $endpos)) }

expr: a = expr PLUS b = expr { Add(a,b) }
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
    | LBRACKET a = expr RBRACKET{ Print a }
    | LPAREN e = expr RPAREN { e }
    | f = ID LPAREN args = separated_list(COMMA, expr) RPAREN { Call(f,args) }
    | LPAREN a = expr COMMA b = expr RPAREN { Cons(a,b) }
    | c = INT { Const c }
    | v = ID { Var v }
    | IF cond = expr THEN bthen = expr ELSE belse = expr { If(cond,bthen,belse) }
    | a = expr SEMICOLON b = expr { Chain(a, b) }
    | error { raise (SyntaxError ("expr", $startpos, $endpos)) }
