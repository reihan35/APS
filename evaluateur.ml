open Ast

type v = InN of Ast.expr
		| InF of  Ast.expr * int * (string*v) list
		| InFR of Ast.expr * int * (string*v) list 
		| ERROR

let v_e n = 
	match n with 
		|Ast.Int n -> n 
		| _ -> -100000000000000 (*Error*)

(*
let rec extend_ctx f args =
	match f with 
		|InF(f,0,l) -> l 
		|InF(f,i,(t,_)::q) ->  (t,List.hd(args))::extend_ctx(InF(e,i-1,q), List.tl(args))
		|InF(f,i,[]) -> print_string("Should not happen") 
*)

let toN v = 
	match v with
		|InN n -> n
		|_-> Ast.Int(-100000000000001) (*(print_string("must be integer");ERROR)*)

let rec rho s env = 
	match env with 
		|[] -> InN(Ast.Int(-10000002))
		|(s,v)::q -> v
		|(x,v)::q -> rho s q


let eval_expr e ctx =
	match e with
		|Ast.Int n -> InN(Ast.Int(n))
		|Ast.Boolean true -> InN(Ast.Int(1))
		|Ast.Boolean false -> InN(Ast.Int(0))
		|Ast.Var s -> v_e(rho(s, ctx))
		|Ast.BinOperation (op,_,_) -> InN(pi(op))
		|Ast.ComOperation (op,_,_) -> InN(pi(op))
		|Ast.BoolOperation (op,_,_) -> InN(pi(op))
		|Ast.UnOperation (op,_)-> InN(pi(op))
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



let pi e = 
	match e with 
		|Ast.BinOperation(s,e,e') -> let e1=eval_expr(e), e2=eval_expr(e') in
									match s with 
									| "add" ->  toN(e1) + toN(e2)
									| "mul" -> toN(e1) * toN(e2)
									| "sub" -> toN(e1) - toN(e2)
									| "div" -> toN(e1) // toN(e2)

		|Ast.Boolean b -> match b with 
							|true -> 1
							|false -> 0

		|Ast.BoolOperation(s,e,e') -> let e1=eval_expr(e) in
									match s with 
									| "and" -> match e1 with
												|InN(0) -> 0
												|InN(1) -> eval_expr(e')
									| "or" -> match e1 with
												|InN(1) -> 1
												|InN(0) -> eval_expr(e')
		
		|Ast.ComOperation(s,e,e') -> let e1=eval_expr(e), e2=eval_expr(e') in
									match s with 
									|"eq" when toN(e1)==toN(e2) -> 1 
									|"eq" -> 0
									|"lt" when toN(e) < toN(e2) -> 1
									|"lt" -> 0

		|Ast.UnOperation(s,e) -> let e1=eval_expr(e) in
								match (s, e1) with 
								|("not", InN(0)) -> 1
								|("not", InN(1)) -> 0


let eval_exprs e ctx = 
	match e with
		|t::q -> eval_expr(t)::eval_exprs(q)
		|t::[] -> eval_expr(t)::[]

let make_closure l  = 
	match l with
		| [] -> []
		| (v,t)::q -> (v,t)::make_closure(q)
		

let eval_dec dec ctx = 
	match d with 
	|FunDec (s, t, l, e) -> (s, InF(e, length(l), make_closure(l)::ctx))
	|ConstDec (s, t, e) -> (s, eval_expr(e))
	|FunRecDec (s, t, l, e) -> (s, InFR(InF(e, length(l), (make_closure(l)::(s, t))::ctx)))


let print_v v = 
	match v with
		|InN n -> print_int(n)
		|InF -> print_string("afficher fermeture")
		|InFR -> print_string("afficher fermeture recursive")

let eval_instr instr ctx =
	match instr with
	|Echo e -> print_v(eval_expr e ctx)

		


