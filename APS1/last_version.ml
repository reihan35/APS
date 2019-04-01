let pi e = 
	match e with 
		|Ast.BinOperation("add",e,e') -> toN(e) + toN(e')
		|Ast.BinOperation("mul",e,e') -> toN(e) * toN(e')
		|Ast.BinOperation("sub",e,e') -> toN(e) - toN(e')
		|Ast.BinOperation("div",e,e') -> toN(e) // toN(e')
		|Ast.BoolOperation("and",Int(0),e') -> 0
		|Ast.BoolOperation("and",Int(1),e') -> e'
		|Ast.BoolOperation("or",Int(1),e') -> 1
		|Ast.BoolOperation("or",Int(0),e') -> e'
		|Ast.ComOperation("eq",e,e') when toN(e1)==toN(e2) -> 1 
		|Ast.ComOperation("eq",e,e') -> 0
		|Ast.ComOperation("lt",e,e') when toN(e) < toN(e2) -> 1
		|Ast.ComOperation("lt",e,e') -> 0 		
		|Ast.UnOperation("not", Int(0)) -> 1
		|Ast.UnOperation("not", Int(1)) -> 0



let toN v = 
	match v with
		|InN n -> n
		|_-> -100000000000001 (*(print_string("must be integer");ERROR)*)

