type_check_args(_,[],[]).
type_check_args(CTX,[TYPE_ARG|T],[ARG|A]) :- typeExpr(CTX,ARG,TYPE_ARG), type_check_args(CTX,T,A).

typeExpr(_, bool(true),bool).
typeExpr(_, bool(false),bool).
typeExpr(_, entier(_),int).
typeExpr(CTX, prim(add, X, Y), int) :- typeExpr(CTX,X,int), typeExpr(CTX,Y,int).
typeExpr(CTX, prim(div, X, Y), int) :- typeExpr(CTX,X,int), typeExpr(CTX,Y,int).
typeExpr(CTX, prim(sub, X, Y), int) :- typeExpr(CTX,X,int), typeExpr(CTX,Y,int).
typeExpr(CTX, prim(mul, X, Y), int) :- typeExpr(CTX,X,int), typeExpr(CTX,Y,int).

typeExpr(CTX, prim(or,X,Y),bool):- typeExpr(CTX,X,bool), typeExpr(CTX,Y,bool). 
typeExpr(CTX, prim(and,X,Y),bool):- typeExpr(CTX,X,bool), typeExpr(CTX,Y,bool). 

typeExpr(CTX, prim(lt,X,Y),bool):- typeExpr(CTX,X,int), typeExpr(CTX,Y,int). 
typeExpr(CTX, prim(eq,X,Y),bool):- typeExpr(CTX,X,int), typeExpr(CTX,Y,int). 
typeExpr(CTX, prim(lt,X,Y),bool):- typeExpr(CTX,X,bool), typeExpr(CTX,Y,bool). 
typeExpr(CTX, prim(eq,X,Y),bool):- typeExpr(CTX,X,bool), typeExpr(CTX,Y,bool). 

typeExpr(CTX, prim(not,X),bool):- typeExpr(CTX,X,bool). 

typeExpr(CTX, prim(nth,X,Y),T):- typeExpr(CTX,X, vec(T)), typeExpr(CTX,Y,int). 
typeExpr(CTX, prim(alloc,X),vec(_)):- typeExpr(CTX,X,int). 
typeExpr(CTX, prim(len,X),int):- typeExpr(CTX,X,vec(_)). 


typeExpr(CTX, ifaps(COND, THEN, ELSE), T) :- typeExpr(CTX, COND, bool), typeExpr(CTX, THEN, T), typeExpr(CTX, ELSE, T). 

typeExpr([(X,T)], var(X), T).
typeExpr([(X,T)|_],var(X), T).
typeExpr([(F, ARGS, RET)|_], var(F), (ARGS, RET)).
typeExpr([_|Z],var(X),T) :- typeExpr(Z,var(X),T).

typeExpr(CTX, funano(arg(ARGS), BODY), (ARGS_TYPE, RET_TYPE)) :- typeArg(CTX, ARGS, NEW_CTX), typeExpr(NEW_CTX, BODY, RET_TYPE), tuple_to_l(ARGS, ARGS_TYPE).

typeExpr(CTX,call([F|R]),TYPE_F) :- typeExpr(CTX,F,(ARGS_TYPE,TYPE_F)),type_check_args(CTX,ARGS_TYPE, R).

typeArg(CTX,[(X, T)], [(X,T)|CTX]).
typeArg(CTX,[(X, T)|Z], [(X,T)|R]) :- typeArg(CTX, Z, R).
typeArg(CTX, [], CTX).

tuple_to_l([],[]).
tuple_to_l([(_, T)], [T]).
tuple_to_l([(_, T)|Z], [T|E]) :- tuple_to_l(Z,E).


typeLval(CTX, var(NAME), T) :- typeExpr(CTX, var(NAME), T).
typeLval(CTX, nth(X, EXPR), T) :- typeLval(CTX, X, vec(T)), typeExpr(CTX, EXPR, int).


/*
type(CTX, typage(bool), bool).
type(CTX, typage(int), int).
type(CTX, typage(T), type(T)).
*/
typeStat(CTX, echo(X), void) :- typeExpr(CTX, X, _).
typeStat(CTX, set(var(NAME), EXPR), void) :- typeExpr(CTX, EXPR, T), typeExpr(CTX, var(NAME), T).
typeStat(CTX, set(X, EXPR), void) :- typeLval(CTX, X, T), typeExpr(CTX, EXPR, T). 

typeStat(CTX, ifstat(COND, THEN, ELSE), void) :- typeExpr(CTX, COND, bool), typeBlock(CTX, THEN, void), typeBlock(CTX, ELSE, void).
typeStat(CTX, whilestat(COND, BODY), void) :- typeExpr(CTX, COND, bool), typeBlock(CTX, BODY, void).
typeStat(CTX, callproc(IDENT, ARGS), void) :- typeExpr(CTX, var(IDENT), (ARGS_TYPE, void)), type_check_args(CTX, ARGS_TYPE, ARGS).


typeDec(CTX,const(X,T,Y),[(X,T)|CTX]) :- typeExpr(CTX, Y, T).

typeDec(CTX,fun(FUN_IDENT,RET_TYPE,arg(ARGS),BODY), [(FUN_IDENT,ARGS_TYPE,RET_TYPE)|CTX])  :- typeArg(CTX, ARGS, NEW_CTX), typeExpr(NEW_CTX, BODY, RET_TYPE), tuple_to_l(ARGS, ARGS_TYPE).

typeDec(CTX,funrec(FUN_IDENT,RET_TYPE,arg(ARGS),BODY), [(FUN_IDENT,ARGS_TYPE,RET_TYPE)|CTX])  :- tuple_to_l(ARGS, ARGS_TYPE), typeArg(CTX, ARGS, NEW_CTX), typeExpr([(FUN_IDENT, ARGS_TYPE, RET_TYPE)|NEW_CTX], BODY, RET_TYPE).

typeDec(CTX, vardec(VARNAME, TYPE), [(VARNAME, TYPE)|CTX]).

typeDec(CTX, procdec(PROC_IDENT, arg(ARGS), BODY), [(PROC_IDENT, ARGS_TYPE, void)|CTX]) :- typeArg(CTX, ARGS, NEW_CTX), typeBlock(NEW_CTX, BODY, void), tuple_to_l(ARGS, ARGS_TYPE).

typeDec(CTX, procrecdec(PROC_IDENT, arg(ARGS), BODY), [(PROC_IDENT, ARGS_TYPE, void)|CTX]) :-  tuple_to_l(ARGS, ARGS_TYPE),  typeArg(CTX, ARGS, NEW_CTX), typeBlock([(PROC_IDENT, ARGS_TYPE, void)|NEW_CTX], BODY, void).


typeCmd(CTX, stat(X), CTX) :- typeStat(CTX, X, void).
typeCmd(CTX, dec(X), NEW_CTX):- typeDec(CTX, X, NEW_CTX).

typeCmds(_, [], void).
typeCmds(CTX, [X|R], void) :- typeCmd(CTX, X, NEW_CTX), typeCmds(NEW_CTX, R, void).

typeBlock(CTX, block(CMDS), void) :- typeCmds(CTX, CMDS, void).

typeProg(prog(X), void) :- typeBlock([], X, void).

main_stdin :-
    read(user_input,T),
    typeProg(T,R) -> print(R); print("echec"),
    nl,
    halt(1).


/*
entier(X).
bool(Z).
prim(CTX,entier(X),entier(Y)).
prim(_,X,Y) :- X, Y.
echo(_).
prog(void).
*/
