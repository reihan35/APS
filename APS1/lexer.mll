{
  open Parser
}
let num = ('-')?['0'-'9']+
let spaces = [' ''\t''\n']
let binop = ("add"|"sub"|"mul"|"div")
let boolop = ("and"|"or")
let unop = ("not")
let compare = ("lt" | "eq")
let ident = ['a'-'z''A'-'Z']['0'-'9''a'-'z''A'-'Z']*
let typing = ("int"|"bool")
let eol = "\n"

rule token = parse
	spaces {token lexbuf}
	| "FUN" {FUN}
	| "CONST" {CONST}
	| "REC" {REC}
	| "ECHO" {ECHO}
	| "true" {TRUE}
	| "false" {FALSE}
	| "if" {IF}
	| "VAR" {VAR}
	| "PROC" {PROC}
	| "SET" {SET}
	| "IF" {IF1}
	| "WHILE" {WHILE}
	| "CALL" {CALL1}
	| "(" {LPAR}
	| ")" {RPAR}
    | "[" {LBRA}
    | "]" {RBRA}
    | "," {COMMA}
    | ";" {SEMICOL}
	| ":" {COL}
	| "*" {STAR}
    | "->" {ARROW}
	| compare as c {COMPARE(c)}
	| typing as t {TPRIM(t)} 
  	| num as x {NUM(int_of_string x)}
	| binop as o {BINOPRIM(o)}
	| boolop as b {BOOLOPRIM(b)}
	| unop as o {UNOPRIM(o)}
	| ident as v {IDENT(v)}
	| eol {EOL}       
