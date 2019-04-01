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

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
open Ast
# 39 "parser.ml"
let yytransl_const = [|
  262 (* EOL *);
  263 (* PLUS *);
  264 (* STAR *);
  265 (* LPAR *);
  266 (* RPAR *);
  267 (* LBRA *);
  268 (* RBRA *);
  269 (* SEMICOL *);
  270 (* COL *);
  271 (* COMMA *);
  272 (* ARROW *);
  273 (* ECHO *);
  274 (* FUN *);
  275 (* CONST *);
  276 (* REC *);
  277 (* TRUE *);
  278 (* FALSE *);
  279 (* IF *);
  280 (* VAR *);
  281 (* PROC *);
  282 (* SET *);
  283 (* IF1 *);
  284 (* WHILE *);
  285 (* CALL1 *);
    0|]

let yytransl_block = [|
  257 (* BINOPRIM *);
  258 (* UNOPRIM *);
  259 (* BOOLOPRIM *);
  260 (* COMPARE *);
  261 (* TPRIM *);
  286 (* NUM *);
  287 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\010\000\011\000\011\000\011\000\005\000\004\000\004\000\
\004\000\004\000\008\000\008\000\009\000\009\000\006\000\007\000\
\007\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\003\000\003\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\002\000\004\000\007\000\
\008\000\003\000\001\000\005\000\001\000\003\000\003\000\001\000\
\003\000\001\000\001\000\001\000\001\000\005\000\005\000\005\000\
\004\000\004\000\004\000\006\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\031\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\018\000\019\000\
\020\000\021\000\006\000\000\000\000\000\000\000\000\000\000\000\
\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\011\000\000\000\000\000\000\000\
\010\000\004\000\005\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\007\000\000\000\025\000\000\000\000\000\000\000\030\000\
\027\000\015\000\017\000\026\000\000\000\000\000\000\000\000\000\
\022\000\024\000\023\000\000\000\000\000\014\000\000\000\000\000\
\028\000\000\000\012\000\008\000\009\000"

let yydgoto = "\002\000\
\004\000\049\000\050\000\010\000\011\000\034\000\035\000\055\000\
\056\000\005\000\012\000"

let yysindex = "\004\000\
\002\255\000\000\249\254\000\000\000\000\253\254\240\254\245\254\
\255\254\019\255\026\255\025\255\057\255\012\255\000\000\000\000\
\000\000\000\000\000\000\013\255\024\255\024\255\024\255\249\254\
\249\254\000\000\253\254\253\254\253\254\253\254\253\254\253\254\
\031\255\033\255\034\255\024\255\000\000\024\255\038\255\253\254\
\000\000\000\000\000\000\253\254\040\255\253\254\253\254\253\254\
\253\254\042\255\024\255\012\255\253\254\043\255\045\255\039\255\
\012\255\000\000\046\255\000\000\052\255\053\255\253\254\000\000\
\000\000\000\000\000\000\000\000\012\255\024\255\024\255\055\255\
\000\000\000\000\000\000\054\255\058\255\000\000\059\255\253\254\
\000\000\253\254\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\060\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\061\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\065\255\000\000\000\000\000\000\000\000\000\000\049\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\022\000\000\000\000\000\000\000\213\255\236\255\
\007\000\000\000\011\000"

let yytablesize = 88
let yytable = "\019\000\
\039\000\040\000\041\000\020\000\001\000\013\000\032\000\014\000\
\067\000\006\000\007\000\008\000\003\000\072\000\021\000\054\000\
\009\000\015\000\016\000\022\000\044\000\045\000\046\000\047\000\
\048\000\077\000\017\000\018\000\037\000\023\000\066\000\024\000\
\038\000\058\000\042\000\043\000\026\000\059\000\025\000\061\000\
\062\000\063\000\033\000\036\000\051\000\053\000\068\000\052\000\
\057\000\060\000\079\000\065\000\070\000\069\000\071\000\073\000\
\076\000\027\000\028\000\029\000\030\000\074\000\075\000\081\000\
\013\000\013\000\080\000\014\000\083\000\082\000\064\000\003\000\
\016\000\084\000\029\000\085\000\078\000\015\000\016\000\031\000\
\000\000\000\000\000\000\000\000\000\000\000\000\017\000\018\000"

let yycheck = "\006\000\
\021\000\022\000\023\000\020\001\001\000\009\001\013\000\011\001\
\052\000\017\001\018\001\019\001\011\001\057\000\031\001\036\000\
\024\001\021\001\022\001\031\001\027\000\028\000\029\000\030\000\
\031\000\069\000\030\001\031\001\005\001\031\001\051\000\013\001\
\009\001\040\000\024\000\025\000\012\001\044\000\013\001\046\000\
\047\000\048\000\031\001\031\001\014\001\012\001\053\000\015\001\
\011\001\010\001\071\000\010\001\008\001\011\001\016\001\010\001\
\063\000\001\001\002\001\003\001\004\001\010\001\010\001\010\001\
\016\001\009\001\012\001\011\001\010\001\012\001\049\000\012\001\
\012\001\080\000\010\001\082\000\070\000\021\001\022\001\023\001\
\255\255\255\255\255\255\255\255\255\255\255\255\030\001\031\001"

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
  VAR\000\
  PROC\000\
  SET\000\
  IF1\000\
  WHILE\000\
  CALL1\000\
  "

let yynames_block = "\
  BINOPRIM\000\
  UNOPRIM\000\
  BOOLOPRIM\000\
  COMPARE\000\
  TPRIM\000\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 20 "parser.mly"
             (Prog(_1))
# 207 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmds) in
    Obj.repr(
# 22 "parser.mly"
                      (Block(_2))
# 214 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 24 "parser.mly"
            (Ast.Stat(_1)::[])
# 221 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 25 "parser.mly"
                    (Ast.Dec(_1)::_3)
# 229 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 26 "parser.mly"
                     (Ast.Stat(_1)::_3)
# 237 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 28 "parser.mly"
                (Echo(_2))
# 244 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.typing) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 30 "parser.mly"
                             (ConstDec(_2, _3, _4))
# 253 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.typing) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : (Ast.arg)list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 31 "parser.mly"
                                       (FunDec(_2, _3, _5, _7))
# 263 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.typing) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : (Ast.arg)list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 32 "parser.mly"
                                           (FunRecDec(_3, _4, _6, _8))
# 273 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typing) in
    Obj.repr(
# 33 "parser.mly"
                   (Var(_2,_3))
# 281 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 35 "parser.mly"
               (Type(_1))
# 288 "parser.ml"
               : Ast.typing))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : (Ast.typing)list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.typing) in
    Obj.repr(
# 36 "parser.mly"
                                 (TypeFun(_2,_4))
# 296 "parser.ml"
               : Ast.typing))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.typing) in
    Obj.repr(
# 38 "parser.mly"
                 (_1::[])
# 303 "parser.ml"
               : (Ast.typing)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.typing) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : (Ast.typing)list) in
    Obj.repr(
# 39 "parser.mly"
                       (_1::_3)
# 311 "parser.ml"
               : (Ast.typing)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typing) in
    Obj.repr(
# 41 "parser.mly"
                       (Arg(_1, _3))
# 319 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 43 "parser.mly"
           (_1::[])
# 326 "parser.ml"
               : (Ast.arg)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : (Ast.arg)list) in
    Obj.repr(
# 44 "parser.mly"
                  (_1::_3)
# 334 "parser.ml"
               : (Ast.arg)list))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
            (Boolean(true))
# 340 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
                                    (Boolean(false))
# 346 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 46 "parser.mly"
                                                           (Int(_1))
# 353 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 46 "parser.mly"
                                                                               (Var(_1))
# 360 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 47 "parser.mly"
                                (BinOperation(_2, _3, _4))
# 369 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 48 "parser.mly"
                               (ComOperation(_2, _3, _4))
# 378 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 49 "parser.mly"
                                 (BoolOperation(_2, _3, _4))
# 387 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 50 "parser.mly"
                          (UnOperation(_2, _3))
# 395 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : (Ast.arg)list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 51 "parser.mly"
                       (AnoFun(_2, _4))
# 403 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : (Ast.expr)list) in
    Obj.repr(
# 52 "parser.mly"
                        (Call(_2,_3))
# 411 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 53 "parser.mly"
                               (If(_3, _4, _5))
# 420 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 55 "parser.mly"
             (_1::[])
# 427 "parser.ml"
               : (Ast.expr)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : (Ast.expr)list) in
    Obj.repr(
# 56 "parser.mly"
              (_1::_2)
# 435 "parser.ml"
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
