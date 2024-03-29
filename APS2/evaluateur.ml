open Ast
open Lexer

exception UninitializedValue
exception UndeclaredVar
exception SegmentationFault

type v = InN of int
       | InF of  Ast.expr * int * (string*v) list
       | InFR of v * string
       | InA of int
       | InP of Ast.block * int * (string*v) list
       | InPR of v * string
       | InB of v * int
       | Unknown





                      

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
  |InB(v,n) -> (print_string("bloc;  "); print_v(v); print_string(" de taille ");print_int(n))
  | Unknown -> print_string("uninitialized\n")

let rec print_mem mem = 
  match mem with 
  | [] -> print_string("\n")
  | t::q -> (print_v(t);print_string(",");print_mem(q))

let rec rho s env = 
  match env with 
  |[] -> raise UndeclaredVar (*L'element n'existe pas dans le contexte*)
  |(e,v)::q when e=s -> v	
  |_::q -> rho s q



let toN v = 
  match v with
  |InN n -> n
  |Unknown -> raise UninitializedValue
  |_-> -1


let rec print_expr e = 
  match e with 
  |Ast.Int x -> print_string(string_of_int x ^ "\n")
  |Ast.Boolean x -> print_string( string_of_bool x)
  |Ast.Prim(s,l) -> (print_string(s); print_exprs(l))
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
  |_->(print_string("Error extend ctx\n"); [])

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
  |_ -> (print_string("Error get body\n"); (Int(1)))

let get_body_p p = 
  match p with 
  |InP(body, _, _) -> body
  |InPR(InP(body, _, _), _)->body
  |_ -> (print_string("Error get body_p\n"); (Block([Stat(Echo(Int(-1)))])))

let rec get_var_value adr mem =
  match (adr,mem) with
  |(0, t::q) -> t
  |(i, t::q) -> get_var_value (i-1) q
  |_->raise SegmentationFault

          
let get_v v mem = 
  match v with
  |InA(adr)-> List.nth mem ((List.length mem) - adr -1)
  |_->v

let rec change_mem i value mem = 
  match (i, mem) with
  |(0, t::q) -> value::q
  |(i, t::q) -> t::(change_mem (i-1) value q)
  |_->(print_string("ERROR");[])

let set x value mem = 
  match x with
  | InA(a) -> let ret = change_mem (List.length(mem) - a - 1) value mem in ret 
  | _ -> (print_string("Can't set non variable types\n"); [])

let rec allocn n mem=
  match n with
  |0 -> mem
  |_ -> Unknown::(allocn (n-1) mem)

let nth b i mem = 
  match b with
  |InB(InA(a),n) -> let i = (List.length(mem) - a - toN(i) - 1) in (List.nth mem i , mem)
  |_ -> (print_string("\nCan't access on non variables / non integer indexes\n");(InN(-1), mem))

let rec get_block v mem=
  match v with
  | InB(a,n)->InB(a,n)
  | InA(a)->let i = (List.length(mem) - a - 1) in List.nth mem i
  |_->(print_string("Address must be InA or InB\n");InA(-1))

let rec lnth v i mem= 
  match v with
  |InB(InA(a),_) ->let res = InA(a+i) in res
  |_ -> (print_string("Can't access on non variables / non integer indexes\n");InN(-1))

let len b mem = 
  match b with 
  |InB(a,n) -> InN(n)
  |_->(print_string("Cannot calculate len of a non type block expression");InN(-1))

let rec eval_expr e mem ctx =
  match e with
  |Ast.Int n -> (InN(n),mem)
  |Ast.Boolean true -> (InN(1),mem)
  |Ast.Boolean false -> (InN(0),mem)
  |Ast.Var s -> let v = rho s ctx in (get_v v mem, mem)
  |Ast.Prim (op,exprs) -> let (v,mem)  = (pi (Prim(op, exprs)) mem ctx) in (v,mem)
  |Ast.If (cnd, thn, el) when let (v,mem) = (eval_expr cnd mem ctx) in (toN(v) == 1) -> (eval_expr thn mem ctx)
  |Ast.If (cnd, thn, el) -> (eval_expr el mem ctx)
  |Ast.Call(fct, args)->let (args,mem) = (eval_exprs args mem ctx) in let (f,mem) = (eval_expr fct mem ctx) in let new_ctx = (extend_ctx f args []) in eval_expr (get_body f) mem new_ctx
  |Ast.AnoFun(typeargs, body) -> ((InF(body, (List.length typeargs), (List.append(make_closure typeargs) ctx))),mem)

and eval_exprs e mem ctx = 
  match e with
  |t::[] -> let (v, mem) = (eval_expr t mem ctx) in (v::[], mem)
  |t::q -> let (v, mem) = (eval_expr t mem ctx) in let (lv, mem) = (eval_exprs q mem ctx) in (v::lv, mem)
  |_-> (print_string("must evaluate at least one expr\n"); ((InN(-1))::[], mem))


and pi prim mem ctx =
  
  match prim with 
  |Prim("add", e1::e2::[]) -> let (left, mem) = (eval_expr e1 mem ctx) in let (right, mem) = (eval_expr e2 mem ctx) in (InN((toN left) + (toN right)), mem)
  |Prim("sub", e1::e2::[]) -> let (left, mem) = (eval_expr e1 mem ctx) in let (right, mem) = (eval_expr e2 mem ctx) in (InN((toN left) - (toN right)),mem)
  |Prim("div", e1::e2::[]) -> let (left, mem) = (eval_expr e1 mem ctx) in let (right, mem) = (eval_expr e2 mem ctx) in (InN((toN left) / (toN right)), mem)
  |Prim("mul", e1::e2::[]) -> let (left, mem) = (eval_expr e1 mem ctx) in let (right, mem) = (eval_expr e2 mem ctx) in (InN((toN left) * (toN right)), mem)
  |Prim("and", e1::e2::[]) -> let (left, mem) = (eval_expr e1 mem ctx) in if ((toN left) == 0) then (InN(0), mem) else let (right, mem) = (eval_expr e2 mem ctx) in (InN((toN right)), mem)
  |Prim("or", e1::e2::[]) -> let (left, mem) = (eval_expr e1 mem ctx) in if ((toN left) == 1) then (InN(1), mem) else let (right, mem) = (eval_expr e2 mem ctx) in (InN(toN right), mem)
  |Prim("eq", e1::e2::[]) -> let (left, mem) = (eval_expr e1 mem ctx) in let (right, mem) = (eval_expr e2 mem ctx) in if ((toN left) == (toN right)) then (InN(1), mem) else (print_v(left);(InN(0), mem))
  |Prim("lt", e1::e2::[]) -> let (left, mem) = (eval_expr e1 mem ctx) in let (right, mem) = (eval_expr e2 mem ctx) in if ((toN left) < (toN right)) then (InN(1), mem) else (InN(0), mem)
  |Prim("not", e1::[]) -> let (left, mem) = (eval_expr e1 mem ctx) in (InN(((toN left) + 1) mod 2), mem)
  |Prim("alloc", e1::[]) -> let (v, mem) = (eval_expr e1 mem ctx) in (InB(InA(List.length(mem)), (toN v)), (allocn (toN v) mem))
  |Prim("nth",e1::e2::[]) -> (let (i,mem) = (eval_expr e2 mem ctx) in let (b,mem) = (eval_expr e1 mem ctx) in (nth b i mem))
  |Prim("len",e1::[]) -> let (b,mem) = (eval_expr e1 mem ctx) in ((len b mem), mem)
  |Prim(prim, _) -> (print_string("wrong prim parameters\n");print_string(prim); (InN(-1),mem))

let rec eval_dec dec mem ctx = 
  match dec with 
  |ConstDec (name, typeret, value) -> (let (v,mem) = (eval_expr value mem ctx) in (mem,((name, v)::ctx)))
  |FunDec (name, typeret, typeargs, body) -> (mem, ((name, (InF(body, (List.length typeargs), (List.append(make_closure typeargs) ctx))))::ctx))
  |FunRecDec (name, typeret, typeargs, body) -> (mem, (name, InFR((InF(body, (List.length typeargs), (List.append(make_closure typeargs) ctx))), name))::ctx)
  |VarDec (name,typage) -> (Unknown::mem, (name,(InA(List.length(mem))))::ctx)
  |ProcDec(name, typeargs, body) -> (mem, (name, (InP(body, (List.length typeargs), (List.append(make_closure typeargs) ctx))))::ctx)
  |ProcRecDec(name,typeargs,body) ->(mem, (name, InPR((InP(body, (List.length typeargs), (List.append(make_closure typeargs) ctx))), name))::ctx)



and eval_instr instr mem ctx =
  match instr with
  |Ast.Echo e -> let (v, mem) = (eval_expr e mem ctx) in (print_v(v);print_string("\n");(mem,ctx))
  |Ast.SetAps(lval,e) -> let (v, mem) = (eval_expr e mem ctx) in let (pos, mem) = (eval_lval lval mem ctx) in ((set pos v mem),ctx)
  |Ast.IfStat(e,b1,b2) -> let (v, mem) = (eval_expr e mem ctx) in if (toN v) == 1 then (eval_block b1 mem ctx) else (eval_block b2 mem ctx)

  |Ast.While(cond, b) -> let (v, mem) = (eval_expr cond mem ctx) in if (toN v)==1 then (let (mem, ctx) = (eval_block b mem ctx) in (eval_instr(While(cond, b)) mem ctx)) else (mem, ctx) 

  |Ast.CallProc(name,args) -> let (args, mem) = (eval_exprs args mem ctx) in let p = (rho name ctx) in let new_ctx = (extend_ctx p args []) in let (mem, _) = eval_block (get_body_p p) mem new_ctx in (mem, ctx)

and eval_lval lval mem ctx = 
  match lval with
|Nth(l, e) -> let (i, mem) = (eval_expr e mem ctx) in let (v, mem) = (eval_lval l mem ctx) in let b = get_block v mem in let res = lnth b (toN i) mem in (res, mem)
  |VarVec(s) -> let v = (rho s ctx) in ((*print_v(v);print_string("\n");print_string("\n");*)(v,mem))

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

