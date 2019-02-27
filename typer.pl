type_check_args(CTX,[],[]).
type_check_args(CTX,[TYPE_ARG],[ARG]) :- typeExpr(CTX,ARG,TYPE_ARG).
type_check_args(CTX,[TYPE_ARG|T],[ARG|A]) :- typeExpr(CTX,ARG,TYPE_ARG), type_check_args(CTX,T,A).

typeExpr(CTX, bool(true),bool).
typeExpr(CTX, bool(false),bool).
typeExpr(CTX, entier(X),int).
typeExpr(CTX, bin_bool_prim(_,X,Y),bool):- typeExpr(CTX,X,bool), typeExpr(CTX,Y,bool). 
typeExpr(CTX, bin_int_prim(_,X,Y),int):- typeExpr(CTX,X,int), typeExpr(CTX,Y,int).
typeExpr(CTX, uni_bool_prim(_,X),bool):- typeExpr(CTX,X,bool). 
typeExpr(CTX, com_prim(_,X,Y),bool):- typeExpr(CTX,X,int), typeExpr(CTX,Y,int).
typeExpr(CTX, ifaps(COND, THEN, ELSE), T) :- typeExpr(CTX, COND, bool), typeExpr(CTX, THEN, T), typeExpr(CTX, ELSE, T). 

typeExpr([(X,T)], var(X), T).
typeExpr([(X,T)|Z],var(X), T).
typeExpr([(F, ARGS, RET)|Z], var(F), (ARGS, RET)).
typeExpr([A|Z],var(X),T) :- typeExpr(Z,var(X),T).
typeExpr(CTX,call([F|R]),TYPE_F) :- typeExpr(CTX,F,(ARGS_TYPE,TYPE_F)),type_check_args(CTX,ARGS_TYPE, R).

typeArg(CTX,[(X, T)], [(X,T)|CTX]).
typeArg(CTX,[(X, T)|Z], [(X,T)|R]) :- typeArg(CTX, Z, R).
typeArg(CTX, [], CTX).

tuple_to_l([],[]).
tuple_to_l([(X, T)], [T]).
tuple_to_l([(X, T)|Z], [T|E]) :- tuple_to_l(Z,E).




/*
type(CTX, typage(bool), bool).
type(CTX, typage(int), int).
type(CTX, typage(T), type(T)).
*/
typeStat(CTX, echo(X), void) :- typeExpr(CTX, X, _).

typeDec(CTX,const(X,T,Y),[(X,T)|CTX]) :- typeExpr(CTX, Y, T).

typeDec(CTX,fun(FUN_IDENT,RET_TYPE,arg(ARGS),BODY), [(FUN_IDENT,ARGS_TYPE,RET_TYPE)|CTX])  :- typeArg(CTX, ARGS, NEW_CTX), typeExpr(NEW_CTX, BODY, RET_TYPE), tuple_to_l(ARGS, ARGS_TYPE).

typeDec(CTX,funrec(FUN_IDENT,RET_TYPE,arg(ARGS),BODY), [(FUN_IDENT,ARGS_TYPE,RET_TYPE)|CTX])  :- tuple_to_l(ARGS, ARGS_TYPE), typeArg(CTX, ARGS, NEW_CTX), typeExpr([(FUN_IDENT, ARGS_TYPE, RET_TYPE)|NEW_CTX], BODY, RET_TYPE).


typeCmd(CTX, stat(X), void) :- typeStat(CTX, X, void).
typeCmd(CTX, dec(X), NEW_CTX):- typeDec(CTX, X, NEW_CTX).



typeProg(CTX, [], void).
typeProg(CTX, [X], void) :- typeCmd(CTX, X, NEW_CTX).
typeProg(CTX, [X|R], void) :- typeCmd(CTX, X, NEW_CTX), typeProg(NEW_CTX, R, void).

prog(X) :- typeProg([], X, void).

/*
entier(X).
bool(Z).
prim(CTX,entier(X),entier(Y)).
prim(_,X,Y) :- X, Y.
echo(_).
prog(void).
*/
