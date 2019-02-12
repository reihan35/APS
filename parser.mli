type token =
  | OPRIM of (string)
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
  | NUM of (int)
  | IDENT of (string)

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.prog
