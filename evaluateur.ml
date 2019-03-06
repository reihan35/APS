open Ast

type V = InN of n
		| InF of  Ast.expr * int * (string,V) list
		| InFR of Ast.expr * int * (string,V) list 
		|ERROR
let v n = 
	match n with 
		|Ast.Int n -> n 
		| _ -> "love you !"

let extend_ctx f args =
	match e with 
		|InF(e,0,l) -> l 
		|InF(e,i,(t,_)::q) ->  (t,hd(args))::call(InF(e,i-1,q), tl(args))
		|InF(e,i,[]) -> print_string("Should not happen") 

let toN v = 
	match v with
		|InN n -> n
		|_-> (print_string("must be integer");ERROR)

let pi e = 
	match e with 
		|Ast.Boolean e match e with 
						|"true" -> 1
						| "false" -> 0

		|Ast.BinOperation(s,e,e') let e1=eval_expr(e), e2=eval_expr(e') in
									match s with 
									| "add" ->  toN(e1) + toN(e2)
									| "mul" -> toN(e1) * toN(e2)
									| "sub" -> toN(e1) - toN(e2)
									| "div" -> toN(e1) // toN(e2)

		|Ast.BoolOperation(s,e,e') let e1=eval_expr(e) in
									match s with 
									| "and" -> match e1 with
												|InN(0) -> 0
												|InN(1) -> eval_expr(e')
									| "or" -> match e1 with
												|InN(1) -> 1
												|InN(0) -> eval_expr(e')
		
		|Ast.ComOperation(s,e,e') let e1=eval_expr(e), e2=eval_expr(e') in
									match s with 
									|"eq" when toN(e1)==toN(e2 -> 1 
									|"eq" -> 0
									|"lt" when toN(e) < toN(e2) -> 1
									|"lt" -> 0

		|Ast.UnOperation(s,e) let e1=eval_expr(e) in
								match (s, e1) with 
								|("not", InN(0)) -> 1
								|("not", InN(1)) -> 0

let rho s env = 
	match env with 
		|[] -> []
		|(s,v)::q -> v
		|(x,v)::q -> rho s q


let eval_exprs e ctx = 
	match e with
		t::q -> eval_expr(t)::eval_exprs(q)
		t::[] -> eval_expr(t)::[]

let make_closure l  = 
	match l with
		| [] -> []
		| (v,t)::q -> (v,_)::make_closure(q)
		

let eval_expr e ctx =
	match e with
		|Ast.Var(s) -> rho(s, ctx)
		|Ast.Int n -> InN(v (n))
		|Ast.Boolean "true" -> InN(1)
		|Ast.Boolean "false" -> InN(0)
		|Ast.BinOperation op -> InN(pi(op))
		|Ast.ComOperation op -> InN(pi(op))
		|Ast.BoolOperation op -> InN(pi(op))
		|Ast.UnOperation op -> InN(pi(op))
		|Ast.If (cnd, thn, el) when eval_expr(cnd) == 1 -> eval_expr(thn)
		|Ast.If (cnd, thn, el) -> eval_expr(el)
		|Ast.Call(f,exprs)-> let args = eval_exprs(exprs, ctx), f = eval_expr(f, ctx) in
									match f with 
										|InF(body, _, _) -> 
											let new_ctx = extend_ctx(f, args) in
												eval_expr(body, new_ctx)
										| InFR(s, fermeture) -> 
											let new_ctx = extend_ctx(fermeture, args) in
												eval_expr(body, (s,fermeture)::new_ctx)
										| _ -> (print_string("L'application doit etre faite sur une fonction");InN(-1))
		|Ast.AnoFun(l,expr) -> InF(expr,length(l),make_closure(l))


let eval_dec dec ctx = 
	match d with 
	|FunDec (s, t, l, e) -> (s, InF(e, length(l), make_closure(l)::ctx))
	|ConstDec (s, t, e) -> (s, eval_expr(e))
	|FunRecDec (s, t, l, e) -> (s, InFR(InF(e, length(l), (make_closure(l)::(s, _))::ctx))


let print_v v = 
	match v with
		|InN n -> print_int(n)
		|InF -> print_string("afficher fermeture")
		|InFR -> print_string("afficher fermeture recursive")

let eval_instr instr ctx =
	match instr with
	|Echo e -> print_v(eval_expr e ctx)

		


