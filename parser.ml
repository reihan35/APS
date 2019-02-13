type token =
  | BINOPRIM of (string)
  | UNOPRIM of (string)
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
# 31 "parser.ml"
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
    0|]

let yytransl_block = [|
  257 (* BINOPRIM *);
  258 (* UNOPRIM *);
  259 (* TPRIM *);
  278 (* NUM *);
  279 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\010\000\010\000\010\000\005\000\004\000\004\000\004\000\
\008\000\008\000\009\000\009\000\006\000\007\000\007\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\003\000\003\000\000\000"

let yylen = "\002\000\
\003\000\001\000\003\000\003\000\002\000\004\000\007\000\008\000\
\001\000\005\000\001\000\003\000\003\000\001\000\003\000\001\000\
\001\000\001\000\001\000\005\000\004\000\004\000\004\000\006\000\
\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\027\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\016\000\017\000\018\000\019\000\
\005\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\009\000\
\000\000\000\000\000\000\003\000\004\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\006\000\000\000\021\000\000\000\026\000\023\000\013\000\
\015\000\022\000\000\000\000\000\000\000\000\000\020\000\000\000\
\000\000\012\000\000\000\000\000\024\000\000\000\010\000\007\000\
\008\000"

let yydgoto = "\002\000\
\004\000\041\000\042\000\008\000\009\000\029\000\030\000\047\000\
\048\000\010\000"

let yysindex = "\004\000\
\001\255\000\000\255\254\000\000\035\255\242\254\245\254\006\255\
\013\255\016\255\030\255\005\255\000\000\000\000\000\000\000\000\
\000\000\011\255\004\255\004\255\255\254\255\254\000\000\035\255\
\035\255\035\255\035\255\015\255\023\255\019\255\004\255\000\000\
\004\255\029\255\035\255\000\000\000\000\035\255\033\255\035\255\
\035\255\037\255\004\255\005\255\035\255\038\255\040\255\042\255\
\005\255\000\000\051\255\000\000\035\255\000\000\000\000\000\000\
\000\000\000\000\005\255\004\255\004\255\050\255\000\000\053\255\
\052\255\000\000\056\255\035\255\000\000\035\255\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\057\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\058\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\061\255\000\000\000\000\000\000\000\000\000\000\059\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\251\255\025\000\000\000\000\000\000\000\215\255\238\255\
\010\000\001\000"

let yytablesize = 73
let yytable = "\017\000\
\034\000\035\000\057\000\018\000\001\000\027\000\032\000\062\000\
\019\000\003\000\033\000\020\000\046\000\005\000\006\000\007\000\
\021\000\065\000\038\000\039\000\040\000\036\000\037\000\022\000\
\056\000\023\000\043\000\028\000\045\000\050\000\024\000\025\000\
\051\000\031\000\053\000\044\000\011\000\049\000\012\000\058\000\
\052\000\011\000\067\000\012\000\055\000\060\000\059\000\064\000\
\013\000\014\000\026\000\015\000\016\000\013\000\014\000\061\000\
\015\000\016\000\063\000\068\000\069\000\070\000\072\000\071\000\
\073\000\054\000\002\000\014\000\025\000\066\000\000\000\000\000\
\011\000"

let yycheck = "\005\000\
\019\000\020\000\044\000\018\001\001\000\011\000\003\001\049\000\
\023\001\009\001\007\001\023\001\031\000\015\001\016\001\017\001\
\011\001\059\000\024\000\025\000\026\000\021\000\022\000\011\001\
\043\000\010\001\012\001\023\001\010\001\035\000\001\001\002\001\
\038\000\023\001\040\000\013\001\007\001\009\001\009\001\045\000\
\008\001\007\001\061\000\009\001\008\001\006\001\009\001\053\000\
\019\001\020\001\021\001\022\001\023\001\019\001\020\001\014\001\
\022\001\023\001\008\001\010\001\008\001\010\001\068\000\008\001\
\070\000\041\000\010\001\010\001\008\001\060\000\255\255\255\255\
\014\001"

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
# 178 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 23 "parser.mly"
            (Ast.Stat(_1)::[])
# 185 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.dec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 24 "parser.mly"
                    (Ast.Dec(_1)::_3)
# 193 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmds) in
    Obj.repr(
# 25 "parser.mly"
                     (Ast.Stat(_1)::_3)
# 201 "parser.ml"
               : 'cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 27 "parser.mly"
                (Echo(_2))
# 208 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.typing) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 29 "parser.mly"
                             (ConstDec(_2, _3, _4))
# 217 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.typing) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : (Ast.arg)list) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 30 "parser.mly"
                                       (FunDec(_2, _3, _5, _7))
# 227 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.typing) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : (Ast.arg)list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 31 "parser.mly"
                                           (FunRecDec(_3, _4, _6, _8))
# 237 "parser.ml"
               : Ast.dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 33 "parser.mly"
               (Type(_1))
# 244 "parser.ml"
               : Ast.typing))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : (Ast.typing)list) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.typing) in
    Obj.repr(
# 34 "parser.mly"
                                 (TypeFun(_2,_4))
# 252 "parser.ml"
               : Ast.typing))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.typing) in
    Obj.repr(
# 36 "parser.mly"
                 (_1::[])
# 259 "parser.ml"
               : (Ast.typing)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.typing) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : (Ast.typing)list) in
    Obj.repr(
# 37 "parser.mly"
                       (_1::_3)
# 267 "parser.ml"
               : (Ast.typing)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typing) in
    Obj.repr(
# 39 "parser.mly"
                       (Arg(_1, _3))
# 275 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 41 "parser.mly"
           (_1::[])
# 282 "parser.ml"
               : (Ast.arg)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : (Ast.arg)list) in
    Obj.repr(
# 42 "parser.mly"
                  (_1::_3)
# 290 "parser.ml"
               : (Ast.arg)list))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
            (Boolean(true))
# 296 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
                                    (Boolean(false))
# 302 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 44 "parser.mly"
                                                           (Int(_1))
# 309 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "parser.mly"
                                                                               (Var(_1))
# 316 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 45 "parser.mly"
                                (BinOperation(_2, _3, _4))
# 325 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 46 "parser.mly"
                          (UnOperation(_2, _3))
# 333 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : (Ast.arg)list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 47 "parser.mly"
                       (AnoFun(_2, _4))
# 341 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : (Ast.expr)list) in
    Obj.repr(
# 48 "parser.mly"
                        (Call(_2,_3))
# 349 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 49 "parser.mly"
                               (If(_3, _4, _5))
# 358 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 51 "parser.mly"
             (_1::[])
# 365 "parser.ml"
               : (Ast.expr)list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : (Ast.expr)list) in
    Obj.repr(
# 52 "parser.mly"
              (_1::_2)
# 373 "parser.ml"
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
