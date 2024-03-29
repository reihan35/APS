open Ast
open Lexer

type v = InN of int
		| InF of  Ast.expr * int * (string*v) list
		| InFR of v * string
		| InA of int
		| InP of Ast.block * int * (string*v) list
		| InPR of v * string
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
		|InA(adr)->(print_string("adresse ");print_int(adr))
		|InP (a, b, c) -> (print_string("afficher fermeture proc : ");print_ctx(c))
		|InPR (v, name) -> (print_string(name);print_string("afficher fermeture recursive proc : ");print_v(v))


let rec print_mem mem = 
	match mem with 
	| [] -> print_string("\n")
	| t::q -> (print_v(t);print_string(",");print_mem(q))

let rec rho s env = 
	match env with 
		|[] -> InN(-10000002) (*L'element n'existe pas dans le contexte*)
		|(e,v)::q when e=s -> v	
		|_::q -> rho s q



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
	|InP(b,0,q) -> (List.append ctx q)
	|InP(b,i,(t,_)::q) -> extend_ctx (InP(b,i-1,q)) (List.tl args) ((t, List.hd args)::ctx)
	|InPR(fermeture, name) -> extend_ctx fermeture args ((name, (InPR(fermeture, name)))::ctx)

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

let get_body_p p = 
	match p with 
	|InP(body, _, _) -> body
    |InPR(InP(body, _, _), _)->body

let rec get_var_value adr mem =
	match (adr,mem) with
	|(0, t::q) -> t
	|(i, t::q) -> get_var_value (i-1) q

let get_v v mem = 
	match v with
	|InA(adr)-> get_var_value ((List.length mem) - adr -1) mem
	|_->v

let rec change_mem i value mem = 
	match (i, mem) with
	|(0, t::q) -> value::q
	|(i, t::q) -> t::(change_mem (i-1) value q)
	|_->(print_string("ERROR");[])

let set x value mem = 
	match x with
	| InA(a) -> change_mem (List.length(mem) - a - 1) value mem 

let rec eval_expr e mem ctx =
	match e with
	|Ast.Int n -> InN(n)
	|Ast.Boolean true -> InN(1)
	|Ast.Boolean false -> InN(0)
	|Ast.Var s -> let v = rho s ctx in get_v v mem
	|Ast.BinOperation (op,e,e') -> let e1=(eval_expr e mem ctx) in let e2=(eval_expr e' mem ctx) in (InN(pi_2 op e1 e2))
	|Ast.ComOperation (op,e,e') ->  let e1=(eval_expr e mem ctx) in let e2=(eval_expr e' mem ctx) in (InN(pi_2 op e1 e2))
	|Ast.BoolOperation (op,e,e') -> let e1=(eval_expr e mem ctx) in let e2=(eval_expr e' mem ctx) in (InN(pi_2 op e1 e2))
	|Ast.UnOperation (op,e)-> let e1=(eval_expr e mem ctx) in (InN (pi_1 e1))
	|Ast.If (cnd, thn, el) when (toN(eval_expr cnd mem ctx)) == 1 -> (eval_expr thn mem ctx)
	|Ast.If (cnd, thn, el) -> (eval_expr el mem ctx)
	|Ast.Call(fct, args)->let args = (eval_exprs args mem ctx) in let f = (eval_expr fct mem ctx) in let new_ctx = (extend_ctx f args []) in eval_expr (get_body f) mem new_ctx
    |Ast.AnoFun(typeargs, body) -> (InF(body, (List.length typeargs), (List.append(make_closure typeargs) ctx)))

and eval_exprs e mem ctx = 
	match e with
	|t::[] -> (eval_expr t mem ctx)::[]
	|t::q -> (eval_expr t mem ctx)::(eval_exprs q mem ctx)

let rec eval_dec dec mem ctx = 
	match dec with 
	|ConstDec (name, typeret, value) -> (mem, (name, (eval_expr value mem ctx))::ctx)
	|FunDec (name, typeret, typeargs, body) -> (mem, (name, (InF(body, (List.length typeargs), (List.append(make_closure typeargs) ctx))))::ctx)
	|FunRecDec (name, typeret, typeargs, body) -> (mem, (name, InFR((InF(body, (List.length typeargs), (List.append(make_closure typeargs) ctx))), name))::ctx)
	|VarDec (name,typage) -> ((InN(0))::mem, (name,(InA(List.length(mem))))::ctx)
	|ProcDec(name, typeargs, body) -> (mem, (name, (InP(body, (List.length typeargs), (List.append(make_closure typeargs) ctx))))::ctx)
	|ProcRecDec(name,typeargs,body) ->(mem, (name, InPR((InP(body, (List.length typeargs), (List.append(make_closure typeargs) ctx))), name))::ctx)



and eval_instr instr mem ctx =
	match instr with
	|Ast.Echo e -> (print_v(eval_expr e mem ctx);print_string("\n");(mem,ctx))
	|Ast.SetAps(s,e) -> ((set (rho s ctx) (eval_expr e mem ctx) mem),ctx)
	|Ast.IfStat(e,b1,b2) when (toN(eval_expr e mem ctx)) == 1 -> (eval_block b1 mem ctx)
	|Ast.IfStat(e,b1,b2) -> eval_block b2 mem ctx
	|Ast.While(cond, b) when (toN (eval_expr cond mem ctx)) == 1 ->  let (newmem, newctx) = (eval_block b mem ctx) in (eval_instr (Ast.While(cond, b)) newmem newctx)
	|Ast.While(cond, b) -> (mem, ctx)
	|Ast.CallProc(name,args) -> let args = (eval_exprs args mem ctx) in let p = (rho name ctx) in let new_ctx = (extend_ctx p args []) in eval_block (get_body_p p) mem new_ctx

and eval_cmd e mem ctx = 
	match e with 
	|Ast.Stat instr -> eval_instr instr mem ctx
	|Ast.Dec dec -> eval_dec dec mem ctx 

and eval_cmds cmds mem ctx = 
	match cmds with 
	|[] -> (mem,ctx)
	|t::q -> let (newmem,newctx) = (eval_cmd t mem ctx) in (eval_cmds q newmem newctx)

and eval_block b mem ctx = 
	match b with 
	|Ast.Block(cmds) -> eval_cmds cmds mem ctx 

let rec eval_prog prog = 
	match prog with 
	|Ast.Prog(b) -> (eval_block b [] [];print_string("\nFin evaluation\n"))


let _ = 
	let fichier = Sys.argv.(1) in
	let f = open_in(fichier) in
	let lexbuf = Lexing.from_channel f in
		try
			let ast = Parser.prog Lexer.token lexbuf in 
				(print_string("\n");
				(eval_prog ast))
		with 
			Parsing.Parse_error -> Printf.printf"Erreur: %d\n" (lexbuf.Lexing.lex_curr_pos)
(*
let _ = 
eval_instr (Ast.Echo(Ast.If(Ast.Boolean(true),Ast.Int(4),Ast.Int(2)))) []
*)

