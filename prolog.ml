open Ast


let rec print_types t = 
	match t with
	| [] -> print_string("")
	| t::r -> (print_type(t);print_types(r))

and print_type t = 
	match t with 
	|Type(s) -> print_string(s)
	|TypeFun([],l) -> print_string("Error")
	|TypeFun(t::q,l) -> (print_string("arrow(");print_type(t);
						print_types(q);print_string(",");
						print_type(l);print_string(")"))

let print_arg a = 
	match a with
	|Arg(s,t) -> (print_string("(");print_string(s);print_string(", ");print_type(t);print_string(")"))

let rec print_args l = 
	match l with
	|[] -> print_string("")
	|t::[] -> print_arg(t)
	|t::q -> (print_arg(t);print_string(","); print_args(q))

(*
let print_oper o =
	match o with 
	|"mul"-> print_string("*")
	|"add"-> print_string("+")
	|"sub"-> print_string("-")
	|"div"-> print_string("//")
*)


let rec print_expr e = 
	match e with 
		|Ast.Int x -> print_string("entier(" ^ string_of_int(x) ^ ")") 
		|Ast.Boolean x -> print_string( string_of_bool x)
		|Ast.ComOperation(s,e, e') -> (print_string("com_prim(");print_string(s);print_string(",");print_expr(e); print_char(',');print_expr(e');print_string(")"))
		|Ast.BinOperation(s,e, e') -> (print_string("bin_int_prim(");print_string(s);print_string(",");print_expr(e); print_char(',');print_expr(e');print_string(")"))
		|Ast.BoolOperation(s,e, e') -> (print_string("bin_bool_prim(");print_string(s);print_string(",");print_expr(e); print_char(',');print_expr(e');print_string(")"))		
		|Ast.UnOperation(s,e) -> (print_string("uni_bool_prim(");print_string(s);print_string(",");print_expr(e);print_string(")"))
		|Ast.AnoFun(args,t) -> ( print_string("funano(");print_args(args); print_string(",");print_expr(t);print_string(")"))
		|Ast.If (cnd,th,el) -> (print_string("ifaps(");print_expr(cnd); print_string(", ");print_expr(th);print_string(",");print_expr(el);print_string(")"))
		|Ast.Var s -> (print_string("var(");print_string(s);print_string(")"))
		|Ast.Call (e, e') -> (print_string("call(");print_string("[");print_expr(e); print_string(",");print_exprs(e');print_string("]"); print_string(")"))
		|Ast.Seq([]) -> print_string("Error")
		|Ast.Seq(t::q) -> (print_expr(t); print_exprs(q))

and print_exprs l = 
	match l with 
	|[] -> print_string("")
	|t::q -> (print_expr(t); print_exprs(q))


let print_stat s = 
	match s with 
	| Echo e -> (print_string("echo(");print_expr(e);print_string(")"))

let print_dec d = 
	match d with 
	|FunDec (s, t, l, e) -> (print_string("fun(");print_string(s);print_string(","); print_type(t); print_string(",");print_string("arg([");print_args(l);print_string("]), ");print_expr(e);print_string(")"))
	|ConstDec (s, t, e) -> (print_string("const(");print_string(s);print_string(",");print_type(t);print_string(",");print_expr(e);print_string(")"))
	|FunRecDec (s, t, l, e) -> (print_string("funrec(");print_string(s);print_string(",");print_type(t);print_string(", ");print_string("arg([");print_args(l);print_string("]), ");print_expr(e);print_string(")"))

let print_cmd c = 
	match c with
	|Ast.Stat s -> (print_string("stat(");print_stat(s); print_string(")"))
	|Ast.Dec d -> (print_string("dec(");print_dec(d); print_string(")"))

let rec print_cmds c = 
	match c with
	|[] -> print_string("")
	|t::[] -> print_cmd(t)
	|t::q -> (print_cmd(t); print_string(",");print_cmds(q))

let rec print_prog p = 
	match p with
	|Ast.Prog(t) -> (print_string("prog([");print_cmds(t);print_string("])."))

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
