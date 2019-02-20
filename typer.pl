typeExpr(CTX, bool(true),bool).
typeExpr(CTX, bool(false),bool).
typeExpr(CTX, entier(X),int).
typeExpr(CTX, bin_bool_prim(_,X,Y),bool):- typeExpr(CTX,X,bool), typeExpr(CTX,Y,bool). 
typeExpr(CTX, bin_int_prim(_,X,Y),int):- typeExpr(CTX,X,int), typeExpr(CTX,Y,int).
typeExpr(CTX, uni_bool_prim(_,X),bool):- typeExpr(CTX,X,bool). 
typeExpr(CTX, com_prim(_,X,Y),bool):- typeExpr(CTX,X,int), typeExpr(CTX,Y,int).
typeExpr(CTX, ifaps(COND, THEN, ELSE), T) :- typeExpr(CTX, COND, bool), typeExpr(CTX, THEN, T), typeExpr(CTX, ELSE, T). 
typeExpr([(X,T)], var(X), T).
typeExpr([(X,T)|Z],var(X),T).
typeExpr([A|Z],var(X),T) :- typeExpr(Z,var(X),T).

type(CTX, typage(bool), bool).
type(CTX, typage(int), int).
type(CTX, typage(T), type(T)).

typeStat(_, echo(X), void) :- typeExpr(CTX, X, _).

typeDec(CTX,const(X,T,Y),[(X,T)|CTX]) :- typeExpr(CTX, Y, T).
typeCmd(_, stat(X), void) :- typeStat(CTX, X, _).
typeCmd(_, dec(X), void) :- typeDec(CTX, X, _).


typeProg(CTX, [X], void) :- typeCmd(CTX, X, void).
typeProg(CTX, [X|R], void) :- typeCmd(CTX, X, void), typeProg(CTX, R, void).

prog(X) :- typeProg([], X, void).

/*
entier(X).
bool(Z).
prim(CTX,entier(X),entier(Y)).
prim(_,X,Y) :- X, Y.
echo(_).
prog(void).
*/
