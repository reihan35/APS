type typing =Type of string
			| TypeFun of typing list * typing


type arg = Arg of string * typing


type expr = Boolean of bool
		| Int of int
		| Operation of string * (expr)list
		| Call of (arg)list * expr
		| If of expr * expr * expr
		| Var of string
		| Seq of expr list

type stat = Echo of expr


type dec = FunDec of string * typing * (arg)list * expr
		| ConstDec of string * typing * expr
		|FunRecDec of string * typing * (arg)list * expr

type cmd = Stat of stat | Dec of dec

type prog = Prog of (cmd)list
 
