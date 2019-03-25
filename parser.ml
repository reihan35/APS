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
  | NUM of (int)
  | IDENT of (string)

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
open Ast
# 33 "parser.ml"
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
    0|]

let yytransl_block = [|
  257 (* BINOPRIM *);
  258 (* UNOPRIM *);
  259 (* BOOLOPRIM *);
  260 (* COMPARE *);
  261 (* TPRIM *);
  280 (* NUM *);
  281 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\010\000\010\000\010\000\005\000\004\000\004\000\004\000\
\008\000\008\000\009\000\009\000\006\000\007\000\007\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\003\000\003\000\000\000"

let yylen = "\002\000\
\003\000\001\000\003\000\003\000\002\000\004\000\007\000\008\000\
\001\000\005\000\001\000\003\000\003\000\001\000\003\000\001\000\
\001\000\001\000\001\000\005\000\005\000\005\000\004\000\004\000\
\004\000\006\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\029\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\016\000\017\000\018\000\019\000\
\005\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\009\000\000\000\000\000\000\000\003\000\004\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\006\000\000\000\023\000\
\000\000\000\000\000\000\028\000\025\000\013\000\015\000\024\000\
\000\000\000\000\000\000\000\000\020\000\022\000\021\000\000\000\
\000\000\012\000\000\000\000\000\026\000\000\000\010\000\007\000\
\008\000"

let yydgoto = "\002\000\
\004\000\045\000\046\000\008\000\009\000\031\000\032\000\051\000\
\052\000\010\000"

let yysindex = "\003\000\
\002\255\000\000\255\254\000\000\044\255\239\254\245\254\012\255\
\013\255\015\255\039\255\003\255\000\000\000\000\000\000\000\000\
\000\000\005\255\000\255\000\255\255\254\255\254\000\000\044\255\
\044\255\044\255\044\255\044\255\044\255\017\255\018\255\022\255\
\000\255\000\000\000\255\025\255\044\255\000\000\000\000\044\255\
\035\255\044\255\044\255\044\255\044\255\036\255\000\255\003\255\
\044\255\040\255\048\255\031\255\003\255\000\000\042\255\000\000\
\047\255\049\255\044\255\000\000\000\000\000\000\000\000\000\000\
\003\255\000\255\000\255\046\255\000\000\000\000\000\000\057\255\
\058\255\000\000\062\255\044\255\000\000\044\255\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\063\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\064\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\067\255\000\000\000\000\000\000\
\000\000\000\000\065\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\251\255\029\000\000\000\000\000\000\000\215\255\238\255\
\012\000\245\255"

let yytablesize = 81
let yytable = "\017\000\
\036\000\037\000\018\000\001\000\034\000\029\000\063\000\019\000\
\035\000\038\000\039\000\068\000\003\000\020\000\050\000\005\000\
\006\000\007\000\040\000\041\000\042\000\043\000\044\000\073\000\
\021\000\022\000\023\000\030\000\062\000\033\000\047\000\054\000\
\048\000\049\000\055\000\053\000\057\000\058\000\059\000\024\000\
\025\000\026\000\027\000\064\000\056\000\061\000\067\000\011\000\
\075\000\012\000\065\000\069\000\011\000\072\000\012\000\066\000\
\070\000\076\000\071\000\013\000\014\000\028\000\015\000\016\000\
\013\000\014\000\077\000\015\000\016\000\078\000\080\000\079\000\
\081\000\060\000\002\000\014\000\027\000\074\000\000\000\000\000\
\011\000"

let yycheck = "\005\000\
\019\000\020\000\020\001\001\000\005\001\011\000\048\000\025\001\
\009\001\021\000\022\000\053\000\011\001\025\001\033\000\017\001\
\018\001\019\001\024\000\025\000\026\000\027\000\028\000\065\000\
\013\001\013\001\012\001\025\001\047\000\025\001\014\001\037\000\
\015\001\012\001\040\000\011\001\042\000\043\000\044\000\001\001\
\002\001\003\001\004\001\049\000\010\001\010\001\016\001\009\001\
\067\000\011\001\011\001\010\001\009\001\059\000\011\001\008\001\
\010\001\012\001\010\001\021\001\022\001\023\001\024\001\025\001\
\021\001\022\001\010\001\024\001\025\001\012\001\076\000\010\001\
\078\000\045\000\012\001\012\001\010\001\066\000\255\255\255\255\
\016\001"

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
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmds) in
    Obj.repr(
# 20 "parser.mly"
                      (Prog(_2))
# 189 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 23 "parser.mly"
            (Ast.Stat(_1)::[])
# 196 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 24 "parser.mly"
                    (Ast.Dec(_1)::_3)
# 204 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 25 "parser.mly"
                     (Ast.Stat(_1)::_3)
# 212 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 27 "parser.mly"
                (Echo(_2))
# 219 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.typing) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 29 "parser.mly"
                             (ConstDec(_2, _3, _4))
# 228 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.typing) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : (Ast.arg)list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 30 "parser.mly"
                                       (FunDec(_2, _3, _5, _7))
# 238 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.typing) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : (Ast.arg)list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 31 "parser.mly"
                                           (FunRecDec(_3, _4, _6, _8))
# 248 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 33 "parser.mly"
               (Type(_1))
# 255 "parser.ml"
               : Ast.typing))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : (Ast.typing)list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.typing) in
    Obj.repr(
# 34 "parser.mly"
                                 (TypeFun(_2,_4))
# 263 "parser.ml"
               : Ast.typing))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.typing) in
    Obj.repr(
# 36 "parser.mly"
                 (_1::[])
# 270 "parser.ml"
               : (Ast.typing)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.typing) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : (Ast.typing)list) in
    Obj.repr(
# 37 "parser.mly"
                       (_1::_3)
# 278 "parser.ml"
               : (Ast.typing)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typing) in
    Obj.repr(
# 39 "parser.mly"
                       (Arg(_1, _3))
# 286 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 41 "parser.mly"
           (_1::[])
# 293 "parser.ml"
               : (Ast.arg)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : (Ast.arg)list) in
    Obj.repr(
# 42 "parser.mly"
                  (_1::_3)
# 301 "parser.ml"
               : (Ast.arg)list))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
            (Boolean(true))
# 307 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
                                    (Boolean(false))
# 313 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 44 "parser.mly"
                                                           (Int(_1))
# 320 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "parser.mly"
                                                                               (Var(_1))
# 327 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 45 "parser.mly"
                                (BinOperation(_2, _3, _4))
# 336 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 46 "parser.mly"
                               (ComOperation(_2, _3, _4))
# 345 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 47 "parser.mly"
                                 (BoolOperation(_2, _3, _4))
# 354 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 48 "parser.mly"
                          (UnOperation(_2, _3))
# 362 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : (Ast.arg)list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 49 "parser.mly"
                       (AnoFun(_2, _4))
# 370 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : (Ast.expr)list) in
    Obj.repr(
# 50 "parser.mly"
                        (Call(_2,_3))
# 378 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 51 "parser.mly"
                               (If(_3, _4, _5))
# 387 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 53 "parser.mly"
             (_1::[])
# 394 "parser.ml"
               : (Ast.expr)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : (Ast.expr)list) in
    Obj.repr(
# 54 "parser.mly"
              (_1::_2)
# 402 "parser.ml"
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
