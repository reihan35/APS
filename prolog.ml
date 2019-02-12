open Ast

let print_arg a = 
	match a with
	|Arg(s,t) -> print_string(s)

let rec print_args l = 
	match l with
	|[] -> print_string("")
	|t::q -> (print_arg(t);print_string(";"); print_args(q))

let print_oper o =
	match o with 
	|"mul"-> print_string("*")
	|"add"-> print_string("+")
	|"sub"-> print_string("-")
	|"div"-> print_string("//")

let rec print_expr e = 
	match e with 
		|Ast.Int x -> print_int(x)
		|Ast.Boolean x -> print_string( string_of_bool x)
		|Ast.Operation(s,t::q) -> (print_string("(");print_expr(t);print_oper(s);print_exprs(q);print_string(")"))
		|Ast.Call(args,t) -> ( print_expr(t);print_string("(");print_args(args);print_string(")"))
		|Ast.If (cnd,th,el) -> (print_expr(cnd); print_string("->");print_expr(th);print_string(";");print_expr(el))
		|Ast.Var s -> print_string(s)
		|Ast.Seq([]) -> print_string("Error")
		|Ast.Seq(t::q) -> (print_expr(t); print_exprs(q))

and print_exprs l = 
	match l with 
	|[] -> print_string("")
	|t::q -> (print_expr(t); print_exprs(q))


let print_stat s = 
	match s with 
	| Echo e -> (print_string("write(");print_expr(e);print_string(")"))

let print_dec d = 
	match d with 
	|FunDec (s, t, l, e) -> (print_string(s);print_args(l);print_expr(e))
	|ConstDec (s, t, e) -> (print_string(s);print_expr(e))
	|FunRecDec (s, t, l, e) -> (print_string(s);print_args(l);print_expr(e))

let print_cmd c = 
	match c with
	|Ast.Stat s -> (print_stat(s);print_string(".\n"))
	|Ast.Dec d -> (print_dec(d);print_string(".\n"))

let rec print_cmds c = 
	match c with
	|[] -> print_string("")
	|t::q -> (print_cmd(t); print_cmds(q))

let rec print_prog p = 
	match p with
	|Ast.Prog(t) -> (print_cmds(t);print_string("\n"))

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
