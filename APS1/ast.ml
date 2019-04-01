type typing =Type of string
		| TypeFun of typing list * typing


type arg = Arg of string * typing


type expr = Boolean of bool
		| Int of int
		| BinOperation of string * expr * expr
		| ComOperation of string * expr * expr
		| BoolOperation of string * expr * expr
		| UnOperation of string * expr
		| AnoFun of (arg)list * expr
		| Call of expr * expr list
		| If of expr * expr * expr
		| Var of string


and stat = Echo of expr
	| SetAps of string * expr
	| IfStat of expr * block * block
	| While of expr * block
	| CallProc of string * expr list

and dec = FunDec of string * typing * (arg)list * expr
		| ConstDec of string * typing * expr
		| FunRecDec of string * typing * (arg)list * expr
		| VarDec of string * typing
		| ProcDec of string * arg list * block
		| ProcRecDec of string * arg list * block

and cmd = Stat of stat | Dec of dec

and block = Block of (cmd)list

type prog = Prog of block
 
