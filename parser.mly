%token <int> INT
%token <string> ID
%token UMINUS
%token PLUS MINUS TIMES DIV POW PIPE SQRT
%token LPAREN RPAREN LBRACKET RBRACKET LACCO RACCO COMMA SEMICOLON COLON
%token FOR TO DO DONE LET EQUALS IF THEN ELSE REC FUN ARROW LEFTARROW IN
%token EOF RETURN VAR DOUBLEEQUALS GREATER LESS GREATEREQUALS LESSEQUALS

%left DOUBLEEQUALS GREATER LESS GREATEREQUALS LESSEQUALS
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left POW
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
    | LPAREN e = expr RPAREN { e }
    | f = ID LPAREN args = separated_list(COMMA, expr) RPAREN { Call(f,args) }
    | LPAREN a = expr COMMA b = expr RPAREN { Cons(a,b) }
    | c = INT { Const c }
    | v = ID { Var v }
    | IF cond = expr THEN bthen = expr ELSE belse = expr { If(cond,bthen,belse) }
    | error { raise (SyntaxError ("expr", $startpos, $endpos)) }
