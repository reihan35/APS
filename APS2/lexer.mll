{
  open Parser
}
let num = ('-')?['0'-'9']+
let spaces = [' ''\t''\n']

let prim = ("not"|"lt"|"eq"|"len"|"alloc"|"and"|"or"|"add"|"sub"|"mul"|"div")
let ident = ['a'-'z''A'-'Z']['0'-'9''a'-'z''A'-'Z']*
let typing = ("int"|"bool"|"void")
let typevec = "vec"
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
        | "nth" {NTH}
	| typing as t {TPRIM(t)}
        | prim as p {PRIM(p)}
        | typevec as p {TYPEVEC(p)}
  	| num as x {NUM(int_of_string x)}
	| ident as v {IDENT(v)}
	| eol {EOL}       
