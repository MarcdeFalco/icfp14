exception Error

type token = 
  | VAR
  | UMINUS
  | TO
  | TL
  | TIMES
  | THEN
  | SQRT
  | SEMICOLON
  | RPAREN
  | RETURN
  | REC
  | RBRACKET
  | RACCO
  | PRINT
  | POW
  | PLUS
  | PIPE
  | MINUS
  | LPAREN
  | LET
  | LESSEQUALS
  | LESS
  | LEFTARROW
  | LBRACKET
  | LACCO
  | INT of (int)
  | IN
  | IF
  | ID of (string)
  | HD
  | GREATEREQUALS
  | GREATER
  | FUN
  | FOR
  | EQUALS
  | EOF
  | ELSE
  | DOUBLEEQUALS
  | DOT
  | DONE
  | DO
  | DIV
  | COMMA
  | COLON
  | ATOM
  | ARROW


val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.t)