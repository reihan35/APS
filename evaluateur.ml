open Ast
open Lexer

type v = InN of int
		| InF of  Ast.expr * int * (string*v) list
		| InFR of v * string
		| ERROR


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


let rec print_ctx ctx = 
	match ctx with 
	| [] -> print_string("\n")
	| (s,v)::q -> (print_string(s);print_string(":");print_v(v);print_string(",");print_ctx(q))

and  print_args l = 
	match l with 
		|[] -> print_string ""
		|(s,v)::q -> (print_string(s);print_v(v);print_args(q)) 

and print_v v = 
	match v with
		|InN n -> print_int(n)
		|InF (a, b, c) -> (print_string("afficher fermeture : ");print_ctx(c))
		|InFR (v, name) -> (print_string(name);print_string("afficher fermeture recursive : ");print_v(v))

let rec rho s env = 
	match env with 
		|[] -> InN(-10000002) (*L'element n'existe pas dans le contexte*)
		|(e,v)::q when e=s -> v
		|_::q -> (print_ctx env; rho s q)



let toN v = 
	match v with
		|InN n -> n
		|_-> -1


let rec print_expr e = 
	match e with 
		|Ast.Int x -> print_string(string_of_int x ^ "\n")
		|Ast.Boolean x -> print_string( string_of_bool x)
		|Ast.UnOperation(s,l) -> (print_string(s); print_expr(l))
		|Ast.If(cnd,th,el) -> (print_string("if");print_expr(cnd);print_expr(th);print_expr(el))
		|Ast.Var s -> print_string(s)
		|_->print_string("")

and print_exprs l = 
	match l with 
	|[] -> print_string ""
	|t::q -> (print_expr t; print_exprs q)

let rec extend_ctx f args ctx =
	match f with 
		|InF(b,0,q) -> (List.append ctx q)
		|InF(b,i,(t,_)::q) -> extend_ctx (InF(b,i-1,q)) (List.tl args) ((t, List.hd args)::ctx)
		|InFR(fermeture, name) -> extend_ctx fermeture args ((name, (InFR(fermeture, name)))::ctx)

let get_name arg =
  match arg with
  |Ast.Arg(name, _)-> name
                                        

let rec make_closure l  = 
	match l with
		| [] -> []
		| t::q -> (get_name t,(InN(0)))::(make_closure q)

let get_body f =
	match f with
	|InF(body, _, _) -> body
        |InFR(InF(body, _, _), _)->body

 

let rec eval_expr e ctx =
	match e with
	|Ast.Int n -> InN(n)
	|Ast.Boolean true -> InN(1)
	|Ast.Boolean false -> InN(0)
	|Ast.Var s -> (print_ctx ctx; print_string s;rho s ctx)
	|Ast.BinOperation (op,e,e') -> let e1=(eval_expr e ctx) in let e2=(eval_expr e' ctx) in (InN(pi_2 op e1 e2))
	|Ast.ComOperation (op,e,e') ->  let e1=(eval_expr e ctx) in let e2=(eval_expr e' ctx) in (InN(pi_2 op e1 e2))
	|Ast.BoolOperation (op,e,e') -> let e1=(eval_expr e ctx) in let e2=(eval_expr e' ctx) in (InN(pi_2 op e1 e2))
	|Ast.UnOperation (op,e)-> let e1=(eval_expr e ctx) in (InN (pi_1 e1))
	|Ast.If (cnd, thn, el) when (toN(eval_expr cnd ctx)) == 1 -> (eval_expr thn ctx)
	|Ast.If (cnd, thn, el) -> (eval_expr el ctx)
	|Ast.Call(fct, args)->(print_ctx(ctx);let args = (eval_exprs args ctx) in let f = (eval_expr fct ctx) in print_v f;let new_ctx = (extend_ctx f args []) in (print_ctx new_ctx); (eval_expr (get_body f) new_ctx))
        |Ast.AnoFun(typeargs, body) -> (InF(body, (List.length typeargs), (List.append(make_closure typeargs) ctx)))

and eval_exprs e ctx = 
	match e with
	|t::[] -> (eval_expr t ctx)::[]
	|t::q -> (eval_expr t ctx)::(eval_exprs q ctx)

let eval_dec dec ctx = 
	match dec with 
	|ConstDec (name, typeret, value) -> (name, (eval_expr value ctx))::ctx
	|FunDec (name, typeret, typeargs, body) -> (name, (InF(body, (List.length typeargs), (List.append(make_closure typeargs) ctx))))::ctx
	|FunRecDec (name, typeret, typeargs, body) -> (name, InFR((InF(body, (List.length typeargs), (List.append(make_closure typeargs) ctx))), name))::ctx

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

