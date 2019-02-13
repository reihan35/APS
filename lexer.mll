{
  open Parser
}
let num = ('-')?['0'-'9']+
let spaces = [' ''\t''\n']
let binop = ("add"|"sub"|"mul"|"div"|"eq"|"lt"|"and"|"or"|"not")
let unop = ("not")
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
	| "(" {LPAR}
	| ")" {RPAR}
    	| "[" {LBRA}
    	| "]" {RBRA}
    	| "," {COMMA}
    	| ";" {SEMICOL}
	| ":" {COL}
	| "*" {STAR}
    	| "->" {ARROW}
	| typing as t {TPRIM(t)} 
  	| num as x {NUM(int_of_string x)}
	| binop as o {BINOPRIM(o)}
	| unop as o {UNOPRIM(o)}
	| ident as v {IDENT(v)}
	| eol {EOL}       
