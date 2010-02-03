

open Ast
open Test_helper
open OUnit

let tests = [
  ("*lhs multi assign",
   [E_Binop(
      E_Tuple([ast_id "x"; E_Unary(Op_UStar,ast_id "y",dp)],dp),
      Op_ASSIGN,
      E_Array([ast_num 1;ast_num 2;ast_num 3;ast_num 4],dp),dp)],
   "(x,*y) = [1,2,3,4]");

  ("nl and array",
   [E_Array([E_Array([ast_num 1],dp)],dp)],
   "[\n[1],\n]");
  
  ("* in cb args",
   [E_MethodCall(ast_id "f",[],
		 Some (E_CodeBlock(false,Some [Formal_star "args"],[],dp)),dp)],
   "f() do |*args| end");

  ("newline array",
  [E_Array([E_Array([ast_num 1; ast_num 2],dp)],dp)],
  "[\n[1, 2]\n]");

  ("class anonymous inheritance",
  [E_ClassDef(E_Empty,
	      Some(Inst_Inherit (lself)),
	      just_body [ltrue], dp)],
   "class << self
     true
    end");

  ("class anonymous inheritance no space",
  [E_ClassDef(E_Empty, 
	      Some(Inst_Inherit (lself)),
	      just_body [], dp)],
   "class <<self
    end");

  ("newline formals",
   [E_MethodDef (ast_id "initialize", 
		[Formal_default("major", lnil);
		 Formal_default("minor", lnil)], just_body [], dp)],
   "def initialize(major = nil,\n minor = nil)\nend");

  ("?? code", [ast_num (Char.code '?')], "??");
  
  ("hex num", [ast_num 0x3F], "0x3F");

  ("<< overload",
  [E_MethodDef(E_Operator(Op_LSHIFT,dp), 
	       [Formal_id (ast_id "element")], 
	       just_body [],dp)],
   "def <<(element) end");

  ("unary - after ?",
   [E_Ternary(E_Block([ast_id "idx"],dp), ast_num (-1), 
	       ast_id "idx",dp)],
   "(idx)? -1: idx");

  ("scoped class name",
   [E_ClassDef(E_Binop(E_Binop(ast_id "Tk", Op_SCOPE, ast_id "BWidget",dp),
		      Op_SCOPE,ast_id "ArrowButton",dp),
	       None, just_body [], dp)],
   "class Tk::BWidget::ArrowButton ; end");

  ("tern newline",
   [E_Ternary(E_Block([ast_id "x"],dp), ast_num 1, ast_num 2,dp)],
   "(x)? \n 1 : 2");

  ("assign tern",
   [E_Binop(ast_id "x",Op_ASSIGN,
	    E_Ternary(E_Block([ltrue],dp), ast_num 1, ast_num 2,dp),
	    dp)],
   "x = (true) ? 1 : 2");

  ("unary - default arg",
   [E_MethodDef(ast_id "f", [Formal_default("x",ast_num (-1))], 
                just_body [], dp)],
   "def f(x = -1) end");

  ("semi after def",
   [E_MethodDef(ast_id "f", [Formal_id (ast_id "z")], 
		just_body [ast_num 1], dp)],
   "def f(z); 1; end");

  ("semi after def",
   [E_MethodDef(ast_id "f", [], just_body [ast_num 1], dp)],
   "def f; 1; end");

  ("formal *",
   [E_MethodDef(ast_id "f", [Formal_rest], just_body [], dp)],
   "def f(*) end");

  ("or return",
   [E_Binop(
      E_Binop(ast_id "x", Op_ASSIGN, ast_id "y",dp),
      Op_kOR,
      E_Return([lfalse],dp),dp)],
   "x = y or return false");

  ("module scope",
   [E_ModuleDef(E_Binop(ast_id "A",Op_SCOPE,ast_id "B",dp),
		just_body [], dp)],
   "module A::B end");
  
  ("multi exp in ()",
   [E_Block([ast_num 1; ast_num 2],dp)],
   "(1;2)");

  ("aset",
  [E_MethodDef(E_Operator(Op_ASET,dp),
	      [Formal_id (ast_id "x"); 
	       Formal_id (ast_id "y")], just_body [], dp)],
   "def []=(x,y) end");

  ("atom =", [ast_atom "foo="], ":foo=");

  ("multi assign last comma",
   [E_Binop(E_Tuple([ast_id "x";E_UOperator(Op_UStar,dp)],dp),
            Op_ASSIGN, ast_num 2,dp)],
   "x, = 2");

  ("inherit self",
   [E_ClassDef(ast_id "C",Some (Class_Inherit (lself)),
	       just_body [],dp)],
   "class C<self; end");

  ("? space char", [ast_num (Char.code ' ')], "?\\s");
  ("? ( char", [ast_num (Char.code '(')], "?\\(");
  ("? ) char", [ast_num (Char.code ')')], "?\\)");

  ("empty case",
   [E_Case({case_guard=E_Empty;case_whens=[([ast_id "@x"],[ast_id "y"])];
	   case_else=[]},dp)], 
   "case\n when @x\n y\n end");

  ("@ atom", [ast_atom "@x"], ":@x");

(*
  ("def %",
   [E_MethodDef(E_Operator Op_REM, Some [Formal_id "x"],[])],
   "def %(x) end");
*)

  ("next method",
   [E_Binop(ast_id "match",Op_ASSIGN,
	    E_MethodCall(E_Binop(ast_id "x", Op_DOT, ast_id "next",dp),[],None,dp),dp)],
   "match = x.next()");

  ("ustar parens", 
  [E_Binop(ast_id "y", Op_ASSIGN, E_Unary(Op_UStar,E_Block([ast_id "x"],dp),dp),
	  dp)], 
  "y = *(x)");
  ("! yield", [E_Unary(Op_UBang,E_Yield([],dp),dp)], "! yield");

  ("assign yield no ()",
   [E_Binop(ast_id "z", Op_ASSIGN,E_Yield([ast_id "@x"],dp),dp)],
   "z = yield @x"
  );

  ("tern after regexp",
   [E_Block([
      E_Ternary(
	E_Binop(ast_id "x", Op_MATCH, ast_re "\\.\\Z" "",dp),
	ast_id "y",
	ast_id "z",dp)],dp)],
   "(x =~ /\\.\\Z/ ? y : z)");

  ("self/class scope",
   [E_Binop(
      E_Binop(lself, Op_SCOPE, ast_id "class",dp),
      Op_SCOPE, ast_id "x",dp)],
   "self::class::x");

  ("nested scope/dot",
   [E_MethodCall(
      E_Binop(
	E_Binop(
	  E_MethodCall(E_Binop(lself,Op_DOT,ast_id "class",dp),
		       [],None,dp),
	  Op_SCOPE,
	  ast_id "Day",dp),
	Op_DOT,
	ast_id "new",dp),
      [ast_id "@maker"],
      None,dp)],
   "self.class::Day.new(@maker)");

  ("binop module",
   [E_Binop(ast_id "x",Op_ASSIGN,ast_num 1,dp);
    E_Binop(ast_id "x",
	   Op_TIMES,
	   E_MethodCall(
	     E_Binop(ast_id "Math",
		     Op_SCOPE,
		     ast_id "sin",dp),
	     [ast_id "y"],
	     None,dp),dp)],
   "x=1;x*Math::sin(y)");

  ("inherit scoped",
   [E_ClassDef(ast_id "X",
      Some(Class_Inherit(E_Unary(Op_UScope,ast_id "Y",dp))),
      just_body [],dp)],
   "class X < ::Y; end");

  ("= paren",
  [E_Binop(
    E_MethodCall(
      E_Binop(lself,Op_DOT,ast_id "x",dp),[],None,dp),
    Op_ASSIGN, E_Block([ast_id "y"],dp),dp)],
  "self.x = (y)");

  ("= no paren call",
  [E_Binop(ast_id "x", Op_ASSIGN,E_MethodCall(ast_id "y", [ast_id "z"], None,dp),dp)],
  "x = y z");
(*
  ("multi block formal",
  [E_MethodCall(ast_id "x",[],
	       Some (E_CodeBlock( of formal_param list * expr list
  "x {|(a, b), c| 3 }");
*)

  (".[]",
   [E_MethodCall(E_Binop(ast_id "x", Op_DOT, E_Operator(Op_AREF,dp),dp),
		[E_Unary(Op_UStar,ast_id "y",dp)],None,dp)],
   "x.[](*y)");

  ("[] screwyness",
   [E_Array([E_MethodCall(ast_id "x", [ast_num 2;ast_num 3], None,dp)],dp)],
   "[x 2,3]");

  ("- overload",
   [E_MethodDef(E_UOperator(Op_UMinus,dp), [Formal_id (ast_id "x")],
		just_body [ast_num 4], dp)],
   "def -@(x) 4 end");

  ("paren brack rhs",
   [E_Binop(ast_id "x", Op_ASSIGN,
	   E_MethodCall(
	       E_Binop(
		   E_Block([ast_id "y"],dp),
		   Op_DOT,
		   E_Operator(Op_AREF,dp),dp),
	       [ast_num 4],None,dp),dp)],
   "x = (y)[4]");

  ("defined?",
   [E_MethodCall(ast_id "defined?",[ast_id "x"],None,dp)],
   "defined?(x)");

  ("x.defined?()",
   [E_Binop(
      E_MethodCall(
	E_Binop(ast_id "x",Op_DOT,ast_id "defined?",dp),
	[ast_id "y"],None,dp),
      Op_kOR,
      ast_id "z",dp)],
   "x.defined?(y) or z");

  ("<<<<",
   [E_Binop(ast_id "x", Op_LSHIFT, doub_str "hi\n",dp)],
   "x <<<<EOM\nhi\nEOM");

  ("class ident",
   [E_Binop(ast_id "@class", Op_ASSIGN, ast_num 3,dp)],
   "@class = 3");

  (":%", [ast_atom "%"], ":%");

  ("alias /",
   [E_Alias((E_Operator(Op_DIV,dp)), ast_id "quo",dp)],
   "alias / quo");

  ("paren command",
   [E_MethodCall(E_Binop(E_Block([ast_id "x"],dp),Op_DOT,ast_id "y",dp),
		 [ast_id "z"],None,dp)],
   "(x).y z");

  ("assign command w/cb",
   [E_Binop(ast_id "x",Op_ASSIGN,
	   E_MethodCall(ast_id "f",[],
		       Some (E_CodeBlock(true,Some [Formal_id  (ast_id "x")],
					[ast_id "x"],dp)
			    )
		       ,dp)
	   ,dp)],
   "x = f  {|x| x}");

  ("*mlhs and mrhs with command",
  [E_Binop(
    E_Unary(Op_UStar,ast_id "a",dp),
    Op_ASSIGN,
    E_MethodCall(ast_id "loop",[],
		Some (E_CodeBlock(false,None,[ast_id "break"],dp)),dp),dp
    )],
  "*a = loop do break; end");

  ("mrhs with *command",
  [E_Binop(
    E_Unary(Op_UStar,ast_id "a",dp),
    Op_ASSIGN,
    E_Unary(Op_UStar,
	   E_MethodCall(ast_id "loop",[],
		       Some (E_CodeBlock(true,None, [ltrue], dp)
			    ),dp),dp),dp
    )],
  "*a = *loop {true}");

  ("nested *rhs",
   [E_Binop(ast_id "a",
	   Op_ASSIGN,
	    E_Unary(Op_UStar,
		    E_Array([E_Unary(Op_UStar, E_Array([],dp),dp)],dp),dp),dp)],
   "a = *[*[]]");

  ("break []",
   [E_MethodCall(ast_id "break",[E_Array([],dp)],None,dp)],
   "break []");

  ("defined command",
   [E_MethodCall(ast_id "defined?",
		 [E_MethodCall(
		   E_Binop(doub_str "a",
			   Op_DOT,
			   ast_id "chomp",dp),
		   [],None,dp)],None,dp)],
   "defined? \"a\".chomp");

  ("command/cb . meth",
   [E_MethodCall(
      E_Binop(
	E_MethodCall(ast_id "map",[],(Some (E_CodeBlock(true,None,[ast_id "x"],dp)
					   )
				     ),dp
		    ),
	  Op_DOT,
	  ast_id "join",dp),
       [],None,dp)],
   "map{x}.join()");

  ("cb inside funcall",
  [E_MethodCall(
     E_Binop(ast_id "x",Op_DOT,ast_id "y",dp),
     [E_MethodCall(
	ast_id "a",[],
	Some (E_CodeBlock(
		true,None,
		[E_MethodCall(ast_id "b",[ast_id "c"],None,dp)],
		dp)),
	dp)
     ],
      None,dp)],
  "x.y(a{b(c)})");

  ("*() in []",
   [E_Binop(E_MethodCall(E_Binop(ast_id "x",Op_DOT,E_Operator(Op_AREF,dp),dp),
			 [E_Unary(Op_UStar,E_Block([ast_id "z"],dp),dp)],None,dp),
	    Op_ASSIGN,
	    ast_id "node",dp)],
   "x[*(z)] = node");

  ("func with *arg",
   [E_MethodCall(ast_id "x",[ast_id "a"; ast_id "b";
			     E_Unary(Op_UStar,ast_id "c",dp)],None,dp)],
   "x(a, b, *c)");

  ("cond in return",
   [E_Return([E_Binop(ast_id "x",Op_kAND,ast_id "y",dp)],dp)],
   "return(x and y)");

  ("pow uminus",
  [E_Binop(ast_id "a",Op_POW,E_Unary(Op_UMinus, ast_id "y",dp),dp)],
  "a ** -y");

  ("mlhs extra ,",
  [E_Binop(E_Tuple([ast_id "x";ast_id "y";E_UOperator(Op_UStar,dp)],dp),
           Op_ASSIGN,ast_id "z",dp)],
  "x, y, = z");

  (". \n",
  [E_MethodCall(E_Binop(ast_id "x",Op_DOT,ast_id "y",dp),[],None,dp)],
  "x.\n  y");

  ("nested commands w/{} cb",
   [E_MethodCall(ast_id "x",
		 [E_MethodCall(ast_id "y",[],
			       Some (E_CodeBlock(true,None,[ast_id "z"],dp)
				    ),dp
			      )],
		 None,dp)],
   "x y{z}");

  ("nested commands w/do cb",
   [E_MethodCall(ast_id "x", [ast_id "y"],
		 Some (E_CodeBlock(false,None,[ast_id "z"],dp)),dp)],
   "x y do z end");

  ("nested 2-arg commands w/{} cb",
   [E_MethodCall(ast_id "x",
		 [ast_id "w";
		  E_MethodCall(ast_id "y",[],
			       Some (E_CodeBlock(true,None,[ast_id "z"],dp)
				    ),dp
			      )],
		 None,dp)],
   "x w, y {z}");

  ("nested 2-args commands w/do cb",
   [E_MethodCall(ast_id "x", [ast_id "w";ast_id "y"],
		 Some (E_CodeBlock(false,None,[ast_id "z"],dp)),dp)],
   "x w, y do z end");

  ("binop command w/ do cb",
   [E_Binop(ast_id "x",
	   Op_ASSOC,
	    E_MethodCall(ast_id "proc",[],
			 Some (E_CodeBlock(false,Some [Formal_id (ast_id "r")],
					   [ast_id "r"],dp)),
			 dp),
	    dp)
   ],
   "x => proc do |r| r end"
  );

  ("scope lhs assign",
   [E_Binop(E_Binop(ast_id "F",Op_SCOPE,ast_id "x",dp),Op_ASSIGN,ast_id "y",dp)],
   "F::x = y");
  
  ("() foram tuple",
  [E_MethodCall(ast_id "d", [], 
	       Some (E_CodeBlock(true,Some [Formal_tuple[Formal_id (ast_id "a");
					       Formal_id (ast_id "b")];
				  Formal_id (ast_id "c")],
				[ast_num 1],dp)
		    )
	       ,dp)],
  "d {|(a,b),c| 1}");

  ("cb after command constant",
  [E_MethodCall(ast_id "dir",[sing_str "a"],
	       Some(E_CodeBlock(false,None,[ast_id "x"],dp)),dp)],
  "dir 'a' do x end");

  ("def (expr).foo()",
  [E_MethodDef(
      E_Binop(E_Binop(ast_id "config",Op_ASSIGN,ast_num 2,dp),
	      Op_DOT, ast_id "z",dp),
      [],just_body [ast_id "x"], dp)],
  "def (config = 2).z(); x end");

  ("nested modifiers",
   [E_Unless(ast_id "z",[E_If(ast_id "y", [ast_id "x"],[],dp)],[],dp)],
   "x if y unless z");

  ("inh expr with assign",
   [E_ClassDef(E_Empty, 
	      Some (Inst_Inherit(E_Binop(ast_id "dig", Op_ASSIGN, 
                                         E_Hash(true,[],dp),dp)
				)
		   ),
	      just_body [],dp)],
   "class << dig = {}; end");

  ("true methodname",
   [E_MethodDef(E_Binop(ast_id "Functions",Op_SCOPE,ltrue,dp),
		[],just_body [ltrue], dp)],
   "def Functions::true( ) true end");

  ("] is not an atom char",
   [E_Binop(ast_id "op",Op_ASSIGN,
	    E_Array([ast_atom ">="],dp),dp)],
   "op = [:>=]"
  );

  (":[] is an atom",
   [E_Binop(ast_id "op",Op_ASSIGN,ast_atom "[]",dp)],
   "op = :[]"
  );

  (":[]= is an atom",
   [E_Binop(ast_id "op",Op_ASSIGN,ast_atom "[]=",dp)],
   "op = :[]="
  );

  ("! method",
   [E_MethodCall(ast_id "id!",[ast_num 2],None,dp)],
   "id!(2)"
  );

  ("id!= is not id! = 2",
  [E_Binop(ast_id "i", Op_NEQ, ast_num 0,dp)],
  "i!=0"
  );

  ("case ;",
  [E_Case({
      case_guard = ast_id "x";
      case_whens = [([ast_id "@y"], [ast_id "z"])];
      case_else = [ast_id "w"];
    }, dp)
  ],
  "case x when @y; z; else; w; end"
  );

  ("hash arguments",
  [E_MethodCall(ast_id "f", 
	       [E_Hash(false,
		   [E_Binop(ast_id "x",Op_ASSOC, ast_num 1,dp);
		    E_Binop(ast_id "y",Op_ASSOC, ast_num 2,dp)
		   ],dp)
	       ], None,dp)],
  "f x => 1, y => 2"
  );

  ("proc argument",
  [E_MethodCall(ast_id "f",[E_Unary(Op_UAmper,ast_id "x",dp)],None,dp)],
  "f(&x)"
  );

  ("default arg isn't parsed as assignable method",
  (let binop = 
    E_Binop(ast_atom "x", Op_ASSOC, ast_num 1,dp)
    in
     [E_MethodDef(ast_id "print",
		 [Formal_default("args", E_Hash(true,[binop],dp))],
		 just_body [], dp)]
  ),
  "def print(args={:x=>1});  end");

  ("return (x).f",
  [E_Return(
    [E_MethodCall(E_Binop(E_Block([ast_id "x"],dp),Op_DOT, ast_id "f", dp),
		 [],None,dp)],dp)],
  "return (x).f");

  ("return (x)/2",
  [E_Return([E_Binop(E_Block([ast_id "x"],dp), Op_DIV, ast_num 2,dp)],dp)],
  "return (x)/2");

  ("newline after brack",
   [E_Binop(
      E_MethodCall(E_Binop(ast_id "y", Op_DOT, E_Operator(Op_AREF,dp),dp),
		   [ast_num 1], None, dp),
      Op_ASSIGN, ast_id "x",dp)
    ],
   "y[\n 1 \n ] = x"
  );

  ("%q is a single string",
   [sing_str " \"&\\##{num};\""],
   "%q~ \"&\\##{num};\"~");

  ("hash literal not args",
  [E_MethodCall(ast_id "f", [E_Hash(false,[
      E_Binop(ast_id "x",Op_ASSOC,ast_num 1,dp);
      E_Binop(ast_id "y",Op_ASSOC,ast_num 2,dp);
    ],dp)],None,dp)],
  "f(x => 1, y => 2)"
  );

  ("hash literal not later args",
  [E_MethodCall(ast_id "f", [ast_num 0;E_Hash(false,[
      E_Binop(ast_id "x",Op_ASSOC,ast_num 1,dp);
      E_Binop(ast_id "y",Op_ASSOC,ast_num 2,dp);
    ],dp)],None,dp)],
    "f(0,x => 1, y => 2)"
  );

  ("hash literal later block",
  [E_MethodCall(ast_id "f", 
                [ast_num 0;
                 E_Hash(false,[
                          E_Binop(ast_id "x",Op_ASSOC,ast_num 1,dp);
                          E_Binop(ast_id "y",Op_ASSOC,ast_num 2,dp);
                        ],dp);
                 E_Unary(Op_UAmper,ast_id "blk",dp);
                ],None,dp)],
    "f(0,x => 1, y => 2,&blk)"
  );

  ("rescue Arg; end",
   [E_ExnBlock({
		 body_exprs = [ast_id "x"];
		 rescue_exprs = [ast_id "ArgumentError", E_Block([],dp)];
		 ensure_expr = [];
		 else_expr = [];
	       },dp)
   ],
   "begin\n x\n rescue ArgumentError; end"
  );

  ("no () multi-arg methodcall with block",
   [E_MethodCall(ast_id "f", [ast_num 1;ast_num 2],
		Some (E_CodeBlock(false,None,[ast_num 3],dp)),dp)],
   "f 1, 2 do 3 end"
  );

  ("rescue unless mods",
   [E_Unless(ast_id "z",
	     [E_ExnBlock({
		body_exprs = [E_MethodCall(ast_id "m", [ast_id "@x"], None, dp)];
		rescue_exprs = [E_Empty,ast_id "y"];
		ensure_expr = []; else_expr = [];
	      },dp)],
	     [],
	     dp)
   ],
   "m @x rescue y unless z"
  );

  ("assign rescue mod",
   [E_Binop(ast_id "x",
	    Op_ASSIGN,
	    E_ExnBlock
	      ({body_exprs = [ast_num 2]; rescue_exprs = [E_Empty,ast_num 4];
		ensure_expr = []; else_expr = [];
	       },dp),
	    dp)],
    "x = 2 rescue 4"
  );

  ("assign if mod",
   [E_If(ast_num 4,
	 [E_Binop(ast_id "x", Op_ASSIGN, ast_num 2,dp)],[],dp)],
   "x = 2 if 4"
  );

  ("massign rescue mod",
   [E_ExnBlock
      ({body_exprs = [E_Binop(E_Tuple([ast_id "x"; ast_id "y"],dp),
			      Op_ASSIGN,
			      E_Tuple([ast_num 2; ast_num 3],dp),dp)];
	rescue_exprs = [E_Empty,ast_num 4];
	ensure_expr = []; else_expr = [];
       },dp)],
   "x,y = 2, 3 rescue 4"
  );

  ("assign op rescue mod",
   [E_ExnBlock
      ({body_exprs = [E_Binop(ast_id "x", Op_OP_ASGN Op_PLUS,
			      E_MethodCall(ast_id "f", [ast_id "x"], None,dp)
                                ,dp)];
	rescue_exprs = [E_Empty,ast_num 4];
	ensure_expr = []; else_expr = [];
       },dp)],
   "x += f(x) rescue 4"
  );

  ("rhs in () makes rescue bind tight",
   [E_Binop(ast_id "foo",
	    Op_ASSIGN,
	    E_ExnBlock({body_exprs = 
			   [E_Block(
			      [E_MethodCall(ast_id "div", [ast_num 3;ast_num 2], 
					    None, dp)],dp)];
			rescue_exprs = [E_Empty,ast_num 7];
			ensure_expr = []; else_expr = []},dp),
	    dp)],
   "foo = (div 3, 2) rescue 7"
  );

  ("rhs w/o () makes rescue bind loose",
   [E_ExnBlock
      ({body_exprs = [E_Binop(ast_id "bar", Op_ASSIGN, 
			      E_MethodCall(ast_id "div", [ast_num 3;ast_num 2], 
					   None, dp),dp)];
	rescue_exprs = [E_Empty,ast_num 7];
	ensure_expr = []; else_expr = [];
       },dp)],
   "bar = div 3, 2 rescue 7"
  );

  ("command on rhs of ||=",
   [E_Binop(ast_id "@z",Op_OP_ASGN Op_OR,E_MethodCall(ast_id "f",
						     [ast_id "@x"],
						     None,dp),dp)],
   "@z ||= f @x"
  );

  ("if rescue modifier",
   [E_ExnBlock({body_exprs = [E_If(ltrue,[ast_id "x"],[],dp)];
		rescue_exprs = [E_Empty, lnil];
		ensure_expr = []; else_expr = []},dp)],
   "x if true rescue nil"
  );

  ("if mod rescue non-mod",
   [E_If(E_ExnBlock({body_exprs = [ltrue]; rescue_exprs = [E_Empty, lnil];
		     ensure_expr = []; else_expr = [];},dp),
	 [ast_id "x"],[],dp)],
   "x if begin true; rescue: nil end"
  );

  ("nl after default arg",
   [E_MethodDef(ast_id "m",
		[Formal_default("x", ast_num 2)],
		just_body [], dp)],
   "def m(x = \n 2) end"
  );

  ("[]= call",
   [E_MethodCall(E_Binop(ast_id "@x",Op_DOT,E_Operator(Op_ASET,dp),dp),
		 [ast_num 1; ast_num 2],None,dp)],
   "@x.[]=(1,2)"
  );

  ("float w/ e no .",
   [ast_float 3.0],
   "3e0"
  );

  ("do binds to the earliest function call",
   [E_MethodCall(ast_id "file",
                 [E_Hash(false,
                         [E_Binop(ast_id "@x",
                                 Op_ASSOC,
                                 E_Binop(ast_id "@y",Op_PLUS,ast_id "@z",dp),
                                 dp)],dp)],
		 Some (E_CodeBlock(false,None,[ast_id "@a"],dp)),dp)],
   "file @x => @y + @z do\n@a\nend\n"
  );

  ("[ after `-string",
   [E_MethodCall(E_Binop(tick_str "ls",Op_DOT,E_Operator(Op_AREF,dp),dp),
                [ast_num 0], None,dp)],
   "`ls`[0]"
  );

  ("command 2-space then [ arg",
   [E_MethodCall(ast_id "f",[E_Array([ast_id "x"],dp);ast_id "y"],None,dp)],
   "f  [x], y"
  );

  ("assign command block",
   [E_Binop(ast_id "x",Op_ASSIGN,E_MethodCall(ast_id "y",[ast_num 3],None,dp),dp)],
   "x = y 3"
  );

  ("assign command arg block",
   [E_Binop(ast_id "w",Op_ASSIGN,
            E_MethodCall(ast_id "x",[ast_id "y"],
                         Some (E_CodeBlock(false,None,[ast_id "z"],dp)),
                         dp),dp)],
   "w = x y do z end"
  );

  ("yield *[]",
   [E_Yield([E_Unary(Op_UStar,E_Array([],dp),dp)],dp)],
   "yield *[]"
  );

  ("bare atom global",
   [ast_atom "$1"],
   ":$1"
  );

  ("heredoc as sole method body",
   [E_MethodDef(ast_id "f",[],just_body [doub_str ""], dp)],
   "def f()\n<<EOF\nEOF\nend"
  );

  (": string can be tern",
   [E_Ternary(ast_id "f",doub_str "'", sing_str "\"",dp)],
   "f ? \"'\":'\"'"
  );

  ("method def ~@",
   [E_MethodDef(E_UOperator(Op_UTilde,dp),[],just_body [ast_num 3], dp)],
   "def ~@() 3 end"
  );

  ("negate inst var",
   [E_Unary(Op_UTilde,ast_id "@x",dp)],
   "~@x"
  );
]

let ignores = [
]

let suite = "Misc Tests" >:::
  List.map simple_test tests @ List.map simple_test_ignore ignores
