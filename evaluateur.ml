open Ast
open Lexer

type v = InN of int
		| InF of  Ast.expr * int * (string*v) list
		| InFR of Ast.expr * int * (string*v) list 
		| ERROR


let rec extend_ctx f args =
	match f with 
		|InF(b,i,[]) -> [] 
		|InF(b,i,(t,_)::q) ->  ((t,List.hd args)::(extend_ctx (InF(b,i-1,q)) (List.tl args)))



let rec rho s env = 
	match env with 
		|[] -> InN(-10000002) (*L'element n'existe pas dans le contexte*)
		|(s,v)::q -> v
		|(x,v)::q -> rho s q


let pi_2 op e1 e2 = 
	match op,e1,e2 with 
		|"add",InN(e1),InN(e2) -> e1 + e2
		|"mul",InN(e1),InN(e2) -> e1 * e2
		|"sub",InN(e1),InN(e2) -> e1 - e2
		|"div",InN(e1),InN(e2) -> e1 / e2
		|"and",InN(0),_ -> 0
		|"and",InN(1),InN(e2)  -> e2 
		|"or",InN(1),_ -> 1
		|"or",InN(0),InN(e2)  -> e2
		|"eq",InN(e1),InN(e2)  when e1==e2 -> 1 
		|"eq",InN(e1),InN(e2) -> 0
		|"lt",InN(e1),InN(e2)  when e1 < e2 -> 1
		|"lt",InN(e1),InN(e2) -> 0 		

let pi_1 e = 
	match e with 
		|InN(0) -> 1
		|InN(1) -> 0


let print_v v = 
	match v with
		|InN n -> print_int(n)
		|InF (a, b, c) -> print_string("afficher fermeture")
		|InFR (a, b, c) -> print_string("afficher fermeture recursive")

let toN v = 
	match v with
		|InN n -> n
		|_-> -1

let get_body f =
	match f with
	|InF(body, _, _) -> body

let rec print_expr e = 
	match e with 
		|Ast.Int x -> print_string(string_of_int x ^ "\n")
		|Ast.Boolean x -> print_string( string_of_bool x)
		|Ast.UnOperation(s,l) -> (print_string(s); print_exprs(l))
		|Ast.If(cnd,th,el) -> (print_string("if");print_expr(cnd);print_expr(th);print_expr(el))
		|Ast.Var s -> print_string(s)
		|_->print_string("")

and print_exprs l = 
	match l with 
	|t::[] -> (print_expr t)
	|t::q -> (print_expr t; print_exprs q)


let rec eval_expr e ctx =
	match e with
	|Ast.Int n -> InN(n)
	|Ast.Boolean true -> InN(1)
	|Ast.Boolean false -> InN(0)
	|Ast.Var s -> (rho s ctx)
	|Ast.BinOperation (op,e,e') -> let e1=(eval_expr e ctx) in let e2=(eval_expr e' ctx) in (InN(pi_2 op e1 e2))
	|Ast.ComOperation (op,e,e') ->  let e1=(eval_expr e ctx) in let e2=(eval_expr e' ctx) in (InN(pi_2 op e1 e2))
	|Ast.BoolOperation (op,e,e') -> let e1=(eval_expr e ctx) in let e2=(eval_expr e' ctx) in (InN(pi_2 op e1 e2))
	|Ast.UnOperation (op,e)-> let e1=(eval_expr e ctx) in (InN (pi_1 e1))
	|Ast.If (cnd, thn, el) when (toN(eval_expr cnd ctx)) == 1 -> (eval_expr thn ctx)
	|Ast.If (cnd, thn, el) -> (eval_expr el ctx)
	|Ast.Call(fct, args)-> (print_exprs args; let args = (eval_exprs args ctx) in let f = (eval_expr fct ctx) in let new_ctx = (extend_ctx f args) in (eval_expr (get_body f) new_ctx))
	(*
	|Ast.Call(, fermeture),exprs)-> let args = (eval_exprs exprs ctx), f = eval_expr(f, ctx) in let new_ctx = (extend_ctx fermeture args) in (eval_expr body (s,fermeture)::new_ctx)
	|Ast.AnoFun(l,expr) -> InF(expr length(l) make_closure(l))*)

and eval_exprs e ctx = 
	match e with
	|t::q -> (eval_expr t ctx)::(eval_exprs q ctx)
	|t::[] -> (eval_expr t ctx)::[]

let get_name arg = 
	match arg with
	|Ast.Arg(name, _)-> name

let rec make_closure l  = 
	match l with
		| [] -> []
		| t::q -> (get_name t,(InN(0)))::(make_closure q)


let eval_dec dec ctx = 
	match dec with 
	|ConstDec (name, typeret, value) -> (name, (eval_expr value ctx))::ctx
	|FunDec (name, typeret, typeargs, body) -> (name, (InF(body, (List.length typeargs), (List.append(make_closure typeargs) ctx))))::ctx
(*
	|FunRecDec (s, t, l, e) -> (s, InFR(InF(e, (List.length l), (make_closure l)::(s, t)::ctx)))

*)

let eval_instr instr ctx =
	match instr with
	|Ast.Echo e -> print_v(eval_expr e ctx)


let eval_cmd e ctx = 
	match e with 
	|Ast.Stat instr -> ((eval_instr instr ctx); ctx)
	|Ast.Dec dec -> eval_dec dec ctx 

let rec eval_cmds cmds ctx = 
	match cmds with 
	|[] -> print_string("")
	|t::q -> let newctx = (eval_cmd t ctx) in (eval_cmds q newctx)



let rec eval_prog prog ctx = 
	match prog with 
	|Ast.Prog(cmds) -> (eval_cmds cmds [])


let _ = 
	let fichier = Sys.argv.(1) in
	let f = open_in(fichier) in
	let lexbuf = Lexing.from_channel f in
		try
			let ast = Parser.prog Lexer.token lexbuf in 
				(print_string("\n");
				(eval_prog ast []))
		with 
			Parsing.Parse_error -> Printf.printf"Erreur: %d\n" (lexbuf.Lexing.lex_curr_pos)
(*
let _ = 
eval_instr (Ast.Echo(Ast.If(Ast.Boolean(true),Ast.Int(4),Ast.Int(2)))) []
*)

