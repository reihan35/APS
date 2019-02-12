open Ast


let rec print_types t = 
	match t with
	| [] -> print_string("")
	| t::r -> (print_type(t);print_types(r))

and print_type t = 
	match t with 
	|Type(s) -> print_string(s)
	|TypeFun([],l) -> print_string("Error")
	|TypeFun(t::q,l) -> (print_string("(");print_type(t);
						print_types(q);print_string("->");
						print_type(l);print_string(")"))

let print_arg a = 
	match a with
	|Arg(s,t) -> (print_string(s);print_type(t))

let rec print_args l = 
	match l with
	|[] -> print_string("")
	|t::q -> (print_arg(t); print_args(q))


let rec print_expr e = 
	match e with 
		|Ast.Int x -> print_string(string_of_int x ^ "\n")
		|Ast.Boolean x -> print_string( string_of_bool x)
		|Ast.Operation(s,l) -> (print_string(s); print_exprs(l))
		|Ast.Call(args,expr) -> (print_args(args); print_expr(expr))
		|Ast.If(cnd,th,el) -> (print_string("if");print_expr(cnd);print_expr(th);print_expr(el))
		|Ast.Var s -> print_string(s)
		|Ast.Seq([]) -> print_string("Error")
		|Ast.Seq(t::q) -> (print_expr(t); print_exprs(q))

and print_exprs l = 
	match l with 
	|[] -> print_string("")
	|t::q -> (print_expr(t); print_exprs(q))

let print_stat s = 
	match s with 
	| Echo e -> print_expr(e)

let print_dec d = 
	match d with 
	|FunDec (s, t, l, e) -> (print_string(s);print_type(t);print_args(l);print_expr(e))
	|ConstDec (s, t, e) -> (print_string(s);print_type(t);print_expr(e))
	|FunRecDec (s, t, l, e) -> (print_string(s);print_type(t);print_args(l);print_expr(e))

let print_cmd c = 
	match c with
	|Ast.Stat s -> print_stat(s)
	|Ast.Dec d -> print_dec(d)

let rec print_cmds c = 
	match c with
	|[] -> print_string("")
	|t::q -> (print_cmd(t); print_cmds(q))

let rec print_prog p = 
	match p with
	|Ast.Prog(t) -> print_cmds(t)

let _ = 
	let fichier = Sys.argv.(1) in
	let f = open_in(fichier) in
	let lexbuf = Lexing.from_channel f in
		try
			let ast = Parser.prog Lexer.token lexbuf in 
				print_string("\n");
				print_prog ast
		with 
			Parsing.Parse_error -> Printf.printf"Erreur: %d\n" (lexbuf.Lexing.lex_curr_pos)
