type typing =Type of string
			| TypeFun of typing list * typing


type arg = Arg of string * typing


type expr = Boolean of bool
		| Int of int
		| BinOperation of string * expr * expr
		| UnOperation of string * expr
		| AnoFun of (arg)list * expr
		| Call of expr * expr list
		| If of expr * expr * expr
		| Var of string
		| Seq of expr list

type stat = Echo of expr


type dec = FunDec of string * typing * (arg)list * expr
		| ConstDec of string * typing * expr
		|FunRecDec of string * typing * (arg)list * expr

type cmd = Stat of stat | Dec of dec

type prog = Prog of (cmd)list
 
