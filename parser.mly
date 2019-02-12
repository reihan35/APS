%{open Ast%}
%token <string> OPRIM
%token <string> TPRIM
%token EOL, PLUS, STAR, LPAR, RPAR, LBRA, RBRA, SEMICOL, COL, COMMA, ARROW, ECHO, FUN, CONST, REC, TRUE, FALSE, IF;
%token <int> NUM;
%token<string> IDENT;
%type<Ast.prog> prog
%type<Ast.expr> expr
%type<(Ast.expr)list> exprs
%type<Ast.dec> dec
%type<Ast.stat> stat
%type<Ast.arg> arg
%type<(Ast.arg)list> args
%type<Ast.typing> typing
%type<(Ast.typing)list> typings
%start prog
%%


prog : LBRA cmds RBRA {Prog($2)}


cmds : stat {Ast.Stat($1)::[]}
	| dec SEMICOL cmds {Ast.Dec($1)::$3}
	| stat SEMICOL cmds {Ast.Stat($1)::$3}

stat: ECHO expr {Echo($2)}

dec: CONST IDENT typing expr {ConstDec($2, $3, $4)} 
	|FUN IDENT typing LBRA args RBRA expr {FunDec($2, $3, $5, $7)}
	|FUN REC IDENT typing LBRA args RBRA expr {FunRecDec($3, $4, $6, $8)}

typing : TPRIM {Type($1)}
	|LPAR typings ARROW typing RPAR {TypeFun($2,$4)}

typings : typing {$1::[]}
	| typing STAR typings {$1::$3}

arg : IDENT COL typing {Arg($1, $3)}

args : arg {$1::[]}
	| arg COMMA args {$1::$3}

expr : TRUE {Boolean(true)} | FALSE {Boolean(false)} | NUM {Int($1)} | IDENT {Var($1)}
	| LPAR OPRIM exprs RPAR {Operation($2, $3)}
	| LBRA args RBRA expr {Call($2, $4)}
	| LPAR expr exprs RPAR {Seq($2::$3)}
	| LPAR IF expr expr expr RPAR {If($3, $4, $5)}

exprs : expr {$1::[]}
	| expr exprs {$1::$2}



