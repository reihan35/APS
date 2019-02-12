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

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
open Ast
# 30 "parser.ml"
let yytransl_const = [|
  259 (* EOL *);
  260 (* PLUS *);
  261 (* STAR *);
  262 (* LPAR *);
  263 (* RPAR *);
  264 (* LBRA *);
  265 (* RBRA *);
  266 (* SEMICOL *);
  267 (* COL *);
  268 (* COMMA *);
  269 (* ARROW *);
  270 (* ECHO *);
  271 (* FUN *);
  272 (* CONST *);
  273 (* REC *);
  274 (* TRUE *);
  275 (* FALSE *);
  276 (* IF *);
    0|]

let yytransl_block = [|
  257 (* OPRIM *);
  258 (* TPRIM *);
  277 (* NUM *);
  278 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\010\000\010\000\010\000\005\000\004\000\004\000\004\000\
\008\000\008\000\009\000\009\000\006\000\007\000\007\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\000\000"

let yylen = "\002\000\
\003\000\001\000\003\000\003\000\002\000\004\000\007\000\008\000\
\001\000\005\000\001\000\003\000\003\000\001\000\003\000\001\000\
\001\000\001\000\001\000\004\000\004\000\004\000\006\000\001\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\026\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\016\000\017\000\018\000\019\000\
\005\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\009\000\000\000\
\000\000\000\000\003\000\004\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\006\000\
\025\000\020\000\000\000\022\000\013\000\015\000\021\000\000\000\
\000\000\000\000\000\000\000\000\000\000\012\000\000\000\000\000\
\023\000\000\000\010\000\007\000\008\000"

let yydgoto = "\002\000\
\004\000\037\000\038\000\008\000\009\000\028\000\029\000\045\000\
\046\000\010\000"

let yysindex = "\013\000\
\009\255\000\000\010\255\000\000\253\254\243\254\011\255\021\255\
\022\255\027\255\029\255\017\255\000\000\000\000\000\000\000\000\
\000\000\019\255\005\255\005\255\010\255\010\255\000\000\253\254\
\253\254\253\254\031\255\032\255\034\255\005\255\000\000\005\255\
\037\255\253\254\000\000\000\000\253\254\045\255\253\254\046\255\
\005\255\017\255\253\254\047\255\049\255\043\255\017\255\000\000\
\000\000\000\000\253\254\000\000\000\000\000\000\000\000\017\255\
\005\255\005\255\048\255\051\255\053\255\000\000\056\255\253\254\
\000\000\253\254\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\055\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\057\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\058\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\054\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\251\255\240\255\000\000\000\000\000\000\222\255\238\255\
\003\000\006\000"

let yytablesize = 67
let yytable = "\017\000\
\033\000\034\000\011\000\018\000\012\000\026\000\031\000\054\000\
\019\000\040\000\032\000\044\000\059\000\001\000\013\000\014\000\
\003\000\015\000\016\000\039\000\049\000\061\000\053\000\005\000\
\006\000\007\000\035\000\036\000\048\000\024\000\021\000\022\000\
\020\000\051\000\011\000\023\000\012\000\055\000\027\000\063\000\
\030\000\041\000\043\000\042\000\047\000\060\000\013\000\014\000\
\025\000\015\000\016\000\050\000\052\000\057\000\056\000\058\000\
\064\000\065\000\068\000\062\000\069\000\066\000\067\000\002\000\
\024\000\014\000\011\000"

let yycheck = "\005\000\
\019\000\020\000\006\001\017\001\008\001\011\000\002\001\042\000\
\022\001\026\000\006\001\030\000\047\000\001\000\018\001\019\001\
\008\001\021\001\022\001\025\000\037\000\056\000\041\000\014\001\
\015\001\016\001\021\000\022\000\034\000\001\001\010\001\010\001\
\022\001\039\000\006\001\009\001\008\001\043\000\022\001\058\000\
\022\001\011\001\009\001\012\001\008\001\051\000\018\001\019\001\
\020\001\021\001\022\001\007\001\007\001\005\001\008\001\013\001\
\009\001\007\001\064\000\057\000\066\000\009\001\007\001\009\001\
\007\001\009\001\013\001"

let yynames_const = "\
  EOL\000\
  PLUS\000\
  STAR\000\
  LPAR\000\
  RPAR\000\
  LBRA\000\
  RBRA\000\
  SEMICOL\000\
  COL\000\
  COMMA\000\
  ARROW\000\
  ECHO\000\
  FUN\000\
  CONST\000\
  REC\000\
  TRUE\000\
  FALSE\000\
  IF\000\
  "

let yynames_block = "\
  OPRIM\000\
  TPRIM\000\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmds) in
    Obj.repr(
# 20 "parser.mly"
                      (Prog(_2))
# 170 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 23 "parser.mly"
            (Ast.Stat(_1)::[])
# 177 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 24 "parser.mly"
                    (Ast.Dec(_1)::_3)
# 185 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 25 "parser.mly"
                     (Ast.Stat(_1)::_3)
# 193 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 27 "parser.mly"
                (Echo(_2))
# 200 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.typing) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 29 "parser.mly"
                             (ConstDec(_2, _3, _4))
# 209 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.typing) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : (Ast.arg)list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 30 "parser.mly"
                                       (FunDec(_2, _3, _5, _7))
# 219 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.typing) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : (Ast.arg)list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 31 "parser.mly"
                                           (FunRecDec(_3, _4, _6, _8))
# 229 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 33 "parser.mly"
               (Type(_1))
# 236 "parser.ml"
               : Ast.typing))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : (Ast.typing)list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.typing) in
    Obj.repr(
# 34 "parser.mly"
                                 (TypeFun(_2,_4))
# 244 "parser.ml"
               : Ast.typing))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.typing) in
    Obj.repr(
# 36 "parser.mly"
                 (_1::[])
# 251 "parser.ml"
               : (Ast.typing)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.typing) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : (Ast.typing)list) in
    Obj.repr(
# 37 "parser.mly"
                       (_1::_3)
# 259 "parser.ml"
               : (Ast.typing)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typing) in
    Obj.repr(
# 39 "parser.mly"
                       (Arg(_1, _3))
# 267 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 41 "parser.mly"
           (_1::[])
# 274 "parser.ml"
               : (Ast.arg)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : (Ast.arg)list) in
    Obj.repr(
# 42 "parser.mly"
                  (_1::_3)
# 282 "parser.ml"
               : (Ast.arg)list))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
            (Boolean(true))
# 288 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
                                    (Boolean(false))
# 294 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 44 "parser.mly"
                                                           (Int(_1))
# 301 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "parser.mly"
                                                                             (Var(_1))
# 308 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : (Ast.expr)list) in
    Obj.repr(
# 45 "parser.mly"
                         (Operation(_2, _3))
# 316 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : (Ast.arg)list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 46 "parser.mly"
                       (Call(_2, _4))
# 324 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : (Ast.expr)list) in
    Obj.repr(
# 47 "parser.mly"
                        (Seq(_2::_3))
# 332 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 48 "parser.mly"
                               (If(_3, _4, _5))
# 341 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 50 "parser.mly"
             (_1::[])
# 348 "parser.ml"
               : (Ast.expr)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : (Ast.expr)list) in
    Obj.repr(
# 51 "parser.mly"
              (_1::_2)
# 356 "parser.ml"
               : (Ast.expr)list))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prog)
