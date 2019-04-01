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
\001\000\010\000\011\000\011\000\011\000\005\000\005\000\005\000\
\005\000\005\000\004\000\004\000\004\000\004\000\004\000\004\000\
\008\000\008\000\009\000\009\000\006\000\007\000\007\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\003\000\003\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\002\000\003\000\004\000\
\003\000\003\000\004\000\007\000\008\000\003\000\006\000\007\000\
\001\000\005\000\001\000\003\000\003\000\001\000\003\000\001\000\
\001\000\001\000\001\000\005\000\005\000\005\000\004\000\004\000\
\004\000\006\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\037\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\024\000\025\000\026\000\027\000\006\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\017\000\
\000\000\000\000\000\000\014\000\000\000\000\000\007\000\000\000\
\009\000\000\000\010\000\004\000\005\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\011\000\000\000\000\000\008\000\036\000\000\000\
\031\000\000\000\000\000\000\000\033\000\021\000\023\000\032\000\
\000\000\000\000\000\000\000\000\000\000\000\000\028\000\030\000\
\029\000\000\000\000\000\020\000\000\000\000\000\000\000\015\000\
\034\000\000\000\018\000\012\000\016\000\013\000"

let yydgoto = "\002\000\
\004\000\058\000\059\000\015\000\016\000\045\000\046\000\072\000\
\073\000\005\000\017\000"

let yysindex = "\004\000\
\012\255\000\000\058\255\000\000\000\000\032\255\011\255\255\254\
\015\255\024\255\026\255\032\255\032\255\030\255\053\255\054\255\
\056\255\017\255\038\255\000\000\000\000\000\000\000\000\000\000\
\040\255\006\255\006\255\006\255\041\255\059\255\032\255\012\255\
\012\255\032\255\058\255\058\255\000\000\032\255\032\255\032\255\
\032\255\032\255\032\255\065\255\066\255\068\255\006\255\000\000\
\006\255\077\255\032\255\000\000\078\255\038\255\000\000\012\255\
\000\000\032\255\000\000\000\000\000\000\032\255\080\255\032\255\
\032\255\032\255\081\255\006\255\038\255\032\255\082\255\084\255\
\079\255\038\255\000\000\038\255\085\255\000\000\000\000\088\255\
\000\000\089\255\091\255\032\255\000\000\000\000\000\000\000\000\
\038\255\006\255\006\255\090\255\092\255\012\255\000\000\000\000\
\000\000\093\255\094\255\000\000\095\255\032\255\012\255\000\000\
\000\000\032\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\096\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\097\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\039\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\098\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\214\255\000\000\000\000\000\000\204\255\238\255\
\017\000\227\255\234\255"

let yytablesize = 114
let yytable = "\024\000\
\067\000\077\000\056\000\057\000\001\000\032\000\033\000\050\000\
\051\000\052\000\048\000\043\000\060\000\061\000\049\000\079\000\
\087\000\038\000\039\000\040\000\041\000\092\000\003\000\093\000\
\055\000\018\000\078\000\019\000\071\000\027\000\025\000\062\000\
\063\000\064\000\065\000\066\000\099\000\020\000\021\000\042\000\
\018\000\026\000\019\000\029\000\075\000\028\000\022\000\023\000\
\035\000\086\000\035\000\035\000\020\000\021\000\030\000\080\000\
\031\000\082\000\083\000\084\000\034\000\022\000\023\000\088\000\
\104\000\035\000\036\000\037\000\044\000\054\000\047\000\053\000\
\101\000\109\000\006\000\007\000\008\000\098\000\068\000\070\000\
\069\000\009\000\010\000\011\000\012\000\013\000\014\000\074\000\
\076\000\081\000\085\000\090\000\089\000\000\000\091\000\108\000\
\094\000\095\000\096\000\110\000\097\000\102\000\105\000\103\000\
\107\000\106\000\100\000\003\000\022\000\000\000\000\000\000\000\
\000\000\019\000"

let yycheck = "\006\000\
\043\000\054\000\032\000\033\000\001\000\012\000\013\000\026\000\
\027\000\028\000\005\001\018\000\035\000\036\000\009\001\058\000\
\069\000\001\001\002\001\003\001\004\001\074\000\011\001\076\000\
\031\000\009\001\056\000\011\001\047\000\031\001\020\001\038\000\
\039\000\040\000\041\000\042\000\089\000\021\001\022\001\023\001\
\009\001\031\001\011\001\020\001\051\000\031\001\030\001\031\001\
\010\001\068\000\012\001\013\001\021\001\022\001\031\001\062\000\
\031\001\064\000\065\000\066\000\031\001\030\001\031\001\070\000\
\094\000\013\001\013\001\012\001\031\001\011\001\031\001\031\001\
\091\000\103\000\017\001\018\001\019\001\084\000\014\001\012\001\
\015\001\024\001\025\001\026\001\027\001\028\001\029\001\011\001\
\011\001\010\001\010\001\008\001\011\001\255\255\016\001\102\000\
\012\001\010\001\010\001\106\000\010\001\012\001\010\001\012\001\
\010\001\012\001\090\000\012\001\012\001\255\255\255\255\255\255\
\255\255\016\001"

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
# 226 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmds) in
    Obj.repr(
# 22 "parser.mly"
                      (Block(_2))
# 233 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 24 "parser.mly"
            (Ast.Stat(_1)::[])
# 240 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 25 "parser.mly"
                    (Ast.Dec(_1)::_3)
# 248 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 26 "parser.mly"
                     (Ast.Stat(_1)::_3)
# 256 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 28 "parser.mly"
                (Echo(_2))
# 263 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 29 "parser.mly"
                  (SetAps(_2, _3))
# 271 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 30 "parser.mly"
                        (IfStat(_2, _3, _4))
# 280 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 31 "parser.mly"
                    (While(_2, _3))
# 288 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : (Ast.expr)list) in
    Obj.repr(
# 32 "parser.mly"
                     (CallProc(_2, _3))
# 296 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.typing) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 34 "parser.mly"
                             (ConstDec(_2, _3, _4))
# 305 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.typing) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : (Ast.arg)list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 35 "parser.mly"
                                       (FunDec(_2, _3, _5, _7))
# 315 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.typing) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : (Ast.arg)list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 36 "parser.mly"
                                           (FunRecDec(_3, _4, _6, _8))
# 325 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typing) in
    Obj.repr(
# 37 "parser.mly"
                   (VarDec(_2,_3))
# 333 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : (Ast.arg)list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 38 "parser.mly"
                                   (ProcDec(_2, _4, _6))
# 342 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : (Ast.arg)list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 39 "parser.mly"
                                       (ProcRecDec(_3, _5, _7))
# 351 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 41 "parser.mly"
               (Type(_1))
# 358 "parser.ml"
               : Ast.typing))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : (Ast.typing)list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.typing) in
    Obj.repr(
# 42 "parser.mly"
                                 (TypeFun(_2,_4))
# 366 "parser.ml"
               : Ast.typing))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.typing) in
    Obj.repr(
# 44 "parser.mly"
                 (_1::[])
# 373 "parser.ml"
               : (Ast.typing)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.typing) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : (Ast.typing)list) in
    Obj.repr(
# 45 "parser.mly"
                       (_1::_3)
# 381 "parser.ml"
               : (Ast.typing)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typing) in
    Obj.repr(
# 47 "parser.mly"
                       (Arg(_1, _3))
# 389 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 49 "parser.mly"
           (_1::[])
# 396 "parser.ml"
               : (Ast.arg)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : (Ast.arg)list) in
    Obj.repr(
# 50 "parser.mly"
                  (_1::_3)
# 404 "parser.ml"
               : (Ast.arg)list))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
            (Boolean(true))
# 410 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
                                    (Boolean(false))
# 416 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 52 "parser.mly"
                                                           (Int(_1))
# 423 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "parser.mly"
                                                                               (Var(_1))
# 430 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 53 "parser.mly"
                                (BinOperation(_2, _3, _4))
# 439 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 54 "parser.mly"
                               (ComOperation(_2, _3, _4))
# 448 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 55 "parser.mly"
                                 (BoolOperation(_2, _3, _4))
# 457 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 56 "parser.mly"
                          (UnOperation(_2, _3))
# 465 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : (Ast.arg)list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 57 "parser.mly"
                       (AnoFun(_2, _4))
# 473 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : (Ast.expr)list) in
    Obj.repr(
# 58 "parser.mly"
                        (Call(_2,_3))
# 481 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 59 "parser.mly"
                               (If(_3, _4, _5))
# 490 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 61 "parser.mly"
             (_1::[])
# 497 "parser.ml"
               : (Ast.expr)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : (Ast.expr)list) in
    Obj.repr(
# 62 "parser.mly"
              (_1::_2)
# 505 "parser.ml"
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
