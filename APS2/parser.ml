type token =
  | PRIM of (string)
  | TPRIM of (string)
  | TYPEVEC of (string)
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
  | NTH
  | NUM of (int)
  | IDENT of (string)

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
open Ast
# 38 "parser.ml"
let yytransl_const = [|
  260 (* EOL *);
  261 (* PLUS *);
  262 (* STAR *);
  263 (* LPAR *);
  264 (* RPAR *);
  265 (* LBRA *);
  266 (* RBRA *);
  267 (* SEMICOL *);
  268 (* COL *);
  269 (* COMMA *);
  270 (* ARROW *);
  271 (* ECHO *);
  272 (* FUN *);
  273 (* CONST *);
  274 (* REC *);
  275 (* TRUE *);
  276 (* FALSE *);
  277 (* IF *);
  278 (* VAR *);
  279 (* PROC *);
  280 (* SET *);
  281 (* IF1 *);
  282 (* WHILE *);
  283 (* CALL1 *);
  284 (* NTH *);
    0|]

let yytransl_block = [|
  257 (* PRIM *);
  258 (* TPRIM *);
  259 (* TYPEVEC *);
  285 (* NUM *);
  286 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\011\000\012\000\012\000\012\000\005\000\005\000\005\000\
\005\000\005\000\004\000\004\000\004\000\004\000\004\000\004\000\
\008\000\008\000\008\000\009\000\009\000\006\000\007\000\007\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\003\000\003\000\010\000\010\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\002\000\003\000\004\000\
\003\000\003\000\004\000\007\000\008\000\003\000\006\000\007\000\
\001\000\004\000\005\000\001\000\003\000\003\000\001\000\003\000\
\001\000\001\000\001\000\001\000\004\000\004\000\004\000\004\000\
\006\000\001\000\002\000\001\000\005\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\038\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\025\000\026\000\027\000\028\000\006\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\036\000\
\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\017\000\
\000\000\000\000\000\000\014\000\000\000\000\000\000\000\007\000\
\000\000\009\000\000\000\010\000\004\000\005\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\011\000\000\000\000\000\000\000\008\000\035\000\
\029\000\000\000\030\000\032\000\022\000\024\000\031\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\018\000\021\000\000\000\000\000\000\000\015\000\037\000\
\033\000\000\000\019\000\012\000\016\000\013\000"

let yydgoto = "\002\000\
\004\000\059\000\060\000\015\000\016\000\045\000\046\000\072\000\
\073\000\033\000\005\000\017\000"

let yysindex = "\010\000\
\009\255\000\000\062\255\000\000\000\000\027\255\242\254\248\254\
\000\255\252\254\254\254\027\255\027\255\014\255\051\255\053\255\
\055\255\012\255\038\255\000\000\000\000\000\000\000\000\000\000\
\040\255\013\255\013\255\013\255\041\255\057\255\046\255\000\000\
\027\255\009\255\009\255\027\255\062\255\062\255\000\000\027\255\
\027\255\027\255\027\255\063\255\067\255\071\255\013\255\000\000\
\036\255\073\255\027\255\000\000\074\255\038\255\254\254\000\000\
\009\255\000\000\027\255\000\000\000\000\000\000\082\255\027\255\
\083\255\084\255\013\255\038\255\027\255\085\255\013\255\087\255\
\088\255\038\255\000\000\038\255\086\255\027\255\000\000\000\000\
\000\000\027\255\000\000\000\000\000\000\000\000\000\000\038\255\
\089\255\013\255\013\255\091\255\093\255\009\255\090\255\096\255\
\095\255\000\000\000\000\098\255\027\255\009\255\000\000\000\000\
\000\000\027\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\097\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\099\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\044\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\094\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\008\000\000\000\000\000\000\000\205\255\238\255\
\009\000\055\000\223\255\022\000"

let yytablesize = 110
let yytable = "\024\000\
\057\000\058\000\077\000\025\000\031\000\034\000\035\000\050\000\
\051\000\052\000\001\000\043\000\040\000\029\000\048\000\026\000\
\086\000\003\000\018\000\049\000\019\000\027\000\092\000\079\000\
\093\000\030\000\056\000\032\000\070\000\028\000\020\000\021\000\
\041\000\018\000\064\000\019\000\097\000\048\000\071\000\042\000\
\022\000\023\000\049\000\036\000\075\000\020\000\021\000\063\000\
\085\000\065\000\066\000\034\000\089\000\034\000\034\000\022\000\
\023\000\082\000\061\000\062\000\103\000\037\000\087\000\038\000\
\039\000\054\000\080\000\044\000\109\000\047\000\053\000\095\000\
\100\000\055\000\067\000\096\000\006\000\007\000\008\000\068\000\
\069\000\074\000\076\000\009\000\010\000\011\000\012\000\013\000\
\014\000\081\000\083\000\084\000\090\000\088\000\108\000\094\000\
\098\000\104\000\099\000\110\000\101\000\091\000\102\000\105\000\
\106\000\107\000\003\000\020\000\023\000\078\000"

let yycheck = "\006\000\
\034\000\035\000\054\000\018\001\007\001\012\000\013\000\026\000\
\027\000\028\000\001\000\018\000\001\001\018\001\002\001\030\001\
\068\000\009\001\007\001\007\001\009\001\030\001\074\000\057\000\
\076\000\030\001\033\000\030\001\047\000\030\001\019\001\020\001\
\021\001\007\001\041\000\009\001\088\000\002\001\003\001\028\001\
\029\001\030\001\007\001\030\001\051\000\019\001\020\001\040\000\
\067\000\042\000\043\000\008\001\071\000\010\001\011\001\029\001\
\030\001\064\000\037\000\038\000\094\000\011\001\069\000\011\001\
\010\001\009\001\059\000\030\001\102\000\030\001\030\001\078\000\
\091\000\028\001\012\001\082\000\015\001\016\001\017\001\013\001\
\010\001\009\001\009\001\022\001\023\001\024\001\025\001\026\001\
\027\001\008\001\008\001\008\001\006\001\009\001\101\000\010\001\
\008\001\008\001\090\000\106\000\010\001\014\001\010\001\008\001\
\010\001\008\001\010\001\014\001\010\001\055\000"

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
  NTH\000\
  "

let yynames_block = "\
  PRIM\000\
  TPRIM\000\
  TYPEVEC\000\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 21 "parser.mly"
             (Prog(_1))
# 221 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmds) in
    Obj.repr(
# 23 "parser.mly"
                      (Block(_2))
# 228 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 25 "parser.mly"
            (Ast.Stat(_1)::[])
# 235 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 26 "parser.mly"
                    (Ast.Dec(_1)::_3)
# 243 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 27 "parser.mly"
                     (Ast.Stat(_1)::_3)
# 251 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 29 "parser.mly"
                (Echo(_2))
# 258 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : (Ast.lval)) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 30 "parser.mly"
                        (SetAps(_2, _3))
# 266 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 31 "parser.mly"
                        (IfStat(_2, _3, _4))
# 275 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 32 "parser.mly"
                    (While(_2, _3))
# 283 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : (Ast.expr)list) in
    Obj.repr(
# 33 "parser.mly"
                     (CallProc(_2, _3))
# 291 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.typing) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 35 "parser.mly"
                             (ConstDec(_2, _3, _4))
# 300 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.typing) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : (Ast.arg)list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 36 "parser.mly"
                                       (FunDec(_2, _3, _5, _7))
# 310 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.typing) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : (Ast.arg)list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 37 "parser.mly"
                                           (FunRecDec(_3, _4, _6, _8))
# 320 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typing) in
    Obj.repr(
# 38 "parser.mly"
                   (VarDec(_2,_3))
# 328 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : (Ast.arg)list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 39 "parser.mly"
                                  (ProcDec(_2, _4, _6))
# 337 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : (Ast.arg)list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 40 "parser.mly"
                                       (ProcRecDec(_3, _5, _7))
# 346 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 42 "parser.mly"
               (Type(_1))
# 353 "parser.ml"
               : Ast.typing))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.typing) in
    Obj.repr(
# 43 "parser.mly"
                                   (TypeVec(_2, _3))
# 361 "parser.ml"
               : Ast.typing))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : (Ast.typing)list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.typing) in
    Obj.repr(
# 44 "parser.mly"
                                 (TypeFun(_2,_4))
# 369 "parser.ml"
               : Ast.typing))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.typing) in
    Obj.repr(
# 46 "parser.mly"
                 (_1::[])
# 376 "parser.ml"
               : (Ast.typing)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.typing) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : (Ast.typing)list) in
    Obj.repr(
# 47 "parser.mly"
                       (_1::_3)
# 384 "parser.ml"
               : (Ast.typing)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typing) in
    Obj.repr(
# 49 "parser.mly"
                       (Arg(_1, _3))
# 392 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 51 "parser.mly"
           (_1::[])
# 399 "parser.ml"
               : (Ast.arg)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : (Ast.arg)list) in
    Obj.repr(
# 52 "parser.mly"
                  (_1::_3)
# 407 "parser.ml"
               : (Ast.arg)list))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
            (Boolean(true))
# 413 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
                                    (Boolean(false))
# 419 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 54 "parser.mly"
                                                           (Int(_1))
# 426 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "parser.mly"
                                                                               (Var(_1))
# 433 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : (Ast.expr)list) in
    Obj.repr(
# 55 "parser.mly"
                        (Prim(_2, _3))
# 441 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : (Ast.expr)list) in
    Obj.repr(
# 56 "parser.mly"
                              (Prim("nth", _3))
# 448 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : (Ast.arg)list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 57 "parser.mly"
                       (AnoFun(_2, _4))
# 456 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : (Ast.expr)list) in
    Obj.repr(
# 58 "parser.mly"
                        (Call(_2,_3))
# 464 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 59 "parser.mly"
                               (If(_3, _4, _5))
# 473 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 61 "parser.mly"
             (_1::[])
# 480 "parser.ml"
               : (Ast.expr)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : (Ast.expr)list) in
    Obj.repr(
# 62 "parser.mly"
              (_1::_2)
# 488 "parser.ml"
               : (Ast.expr)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "parser.mly"
            (VarVec(_1))
# 495 "parser.ml"
               : (Ast.lval)))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : (Ast.lval)) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 65 "parser.mly"
                                  (Nth(_3, _4))
# 503 "parser.ml"
               : (Ast.lval)))
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
