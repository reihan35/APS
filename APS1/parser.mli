type token =
  | BINOPRIM of (string)
  | UNOPRIM of (string)
  | BOOLOPRIM of (string)
  | COMPARE of (string)
  | TPRIM of (string)
  | EOL
  | PLUS
  | STAR
  | LPAR
  | RPAR
  | LBRA
  | RBRA
  | SEMICOL
  | COL
  | COMMA
  | ARROW
  | ECHO
  | FUN
  | CONST
  | REC
  | TRUE
  | FALSE
  | IF
  | VAR
  | PROC
  | SET
  | IF1
  | WHILE
  | CALL1
  | NUM of (int)
  | IDENT of (string)

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.prog
