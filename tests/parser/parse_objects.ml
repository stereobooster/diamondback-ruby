

open Ast
open Test_helper
open OUnit

let tests = [
  ("module",
  [E_ModuleDef(ast_id "Modname", just_body [ltrue;lfalse], dp)],
   "module Modname\n true\n false end");

  ("class def",
   [E_ClassDef(ast_id "Clazz",None,just_body [ltrue], dp)],
   "class Clazz; true end");

  ("method def",
   [E_MethodDef(ast_id "foo", [], just_body [ltrue], dp)],
   "def foo() true end");
  
  ("method def with args",
   [E_MethodDef(ast_id "foo", 
		[Formal_id (ast_id "arg1"); Formal_id (ast_id "arg2");],
		just_body [ltrue], dp)],
   "def foo(arg1,arg2) true end");

  ("method def assignable",
  [E_MethodDef(E_Binop(lself,Op_DOT,ast_id "x=",dp),
	      [Formal_id (ast_id "v")],
	      just_body[E_Binop(ast_id "@x",Op_ASSIGN, ast_id "v",dp)],
	       dp)],
  "def self.x=(v) @x = v end");

  ("method def scoped",
  [E_MethodDef(
    E_Binop(ast_id "Benchmark", Op_SCOPE, ast_id "times",dp),
     [],just_body [ltrue],dp)],
  "def Benchmark::times() true end"
  );

  ("method def paren starts body",
   [E_MethodDef(ast_id "f", [Formal_id (ast_id "x")], 
	       just_body [E_Block([E_Binop(ast_id "x", Op_PLUS, ast_num 1,dp)],dp)],
		dp)],
   "def f(x) (x + 1) end");

  ("method scoped assign",
   [E_MethodDef(E_Binop(ast_id "F",Op_SCOPE,
			ast_id "g=",dp),
	       [Formal_id (ast_id "x")],
	       just_body [ast_id "x"], dp)],
   "def F::g=(x) ; x; end");

  ("method def %",
  [E_MethodDef(E_Operator(Op_REM,dp),
	      [Formal_id (ast_id "other")], 
	      just_body [ltrue], dp)],
  "def %(other) true end");

  ("class and method",
   [E_ClassDef(
      ast_id "Clazz",
      None,
      just_body [E_MethodDef(ast_id "foo", 
			     [Formal_id (ast_id "arg1"); 
			      Formal_id (ast_id "arg2");],
			     just_body [ltrue], dp)], 
      dp)],
   "class Clazz; def foo(arg1,arg2) true end end");

  ("class inherit dot",
   [E_ClassDef(E_Empty,
	       Some (Inst_Inherit 
		       (E_MethodCall(E_Binop(ast_id "@x",Op_DOT,ast_id "y",dp),
				     [], None,dp))),
	       just_body [ltrue], dp
	      )],
   "class<<@x.y;true;end");
  
  ("class inherit method",
   [E_ClassDef(ast_id "X",
	       Some (Class_Inherit
		       (E_MethodCall(ast_id "y",[ast_id "z"],None,dp))),
	       just_body [ltrue], dp
	      )],
   "class X<y(z); true end");

  ("method call",
   [E_MethodCall(ast_id "id1", [], None,dp)],
   "id1()");

  ("method call arg",
   [E_MethodCall(ast_id "id1", [ast_id "x"], None, dp)],
   "id1(x)\n");

  ("method call arg",[E_MethodCall(ast_id "id1", [ast_id "x"], None, dp)],
   "id1 x\n");

  ("method call arg",
   [E_MethodCall(ast_id "id1",[E_Binop(ast_id "x", Op_PLUS, ast_num 2, dp)], None,dp)],
   "id1 x+2");

  ("method call args",
   [E_MethodCall(ast_id "id1",[ast_id "x";ast_id "y";ast_id "z"], None,dp)],
   "id1(x,y,z)\n");

  ("method call args",
   [E_MethodCall(ast_id "id1",[ast_id "x"; ast_id "y"; ast_id "z"], None,dp)],
   "id1 x,y,z\n");

  ("method call paren",
   [E_MethodCall(
      E_Binop(E_Block([E_Binop(ast_id "x", Op_PLUS, ast_num 2,dp)],dp),
	      Op_DOT,ast_id "y",dp)
	,[], None,dp)],
   "(x+2).y");

  ("method call hash literal",
   [E_MethodCall(
      E_Binop(
	E_MethodCall(
	  E_Binop(E_Hash(true,[],dp), Op_DOT, ast_id "y",dp),
	  [],
	  None,dp),
	Op_DOT,ast_id "z",dp),
      [], None,dp)],
   "{}.y.z");

  ("nested method calls",
   [E_MethodCall(
      E_Binop(E_MethodCall(ast_id "a", [ast_id "b"], None,dp),
	      Op_DOT,
	      ast_id "c",dp),
      [ast_id "d"],
      None,dp
    )],
   "a(b).c(d)");

  ("method call block",
   [E_MethodCall (ast_id "proc",[], 
		  Some (E_CodeBlock(true,Some [Formal_id (ast_id "x")], 
				    [ast_id "x"],dp)),
		 dp)],
   "proc {|x| x}");

  ("method call block",
   [E_MethodCall(ast_id "id1",[ast_id "x"], 
		 Some (E_CodeBlock(true,Some [Formal_id (ast_id "x")], 
				   [ast_id "x"],dp)),dp)],
   "id1(x) {|x| x }\n");

  ("method call block comma",
   [E_MethodCall(ast_id "id1",[ast_id "x"], 
		 Some (E_CodeBlock(true, Some [Formal_id (ast_id "x"); Formal_rest],
				   [ast_id "x"],dp)),dp)],
   "id1(x) {|x,| x }\n");

  ("nested method block",
   begin
     let id x = Some(E_CodeBlock(true,Some [Formal_id (ast_id x)], [ast_id x],dp)) in
     let first = E_MethodCall(ast_id "a", [ast_id "b"], id "c",dp) in
     let second = E_Binop(first, Op_DOT, ast_id "d",dp) in
     let third = E_MethodCall(second, [ast_id "e"], id "f",dp) in
       [E_MethodCall(E_Binop(third, Op_DOT, ast_id "g",dp),[],None,dp)]
   end,
  "a(b) {|c| c}.d(e) {|f| f}.g");

  ("method embedded assign",
   [E_MethodCall(ast_id "x", [ast_id "z";
			      E_Binop(ast_id "y",
				      Op_ASSIGN,
				      ast_num 2,dp)],
		 
		 None,dp)],
   "x(z,y=2)");

  ("method call *args",
  [E_MethodCall(ast_id "f",
	       [E_Unary(Op_UStar,
		       E_MethodCall(ast_id "x",[ast_id "y"],None,dp),dp)],
	       None,dp)],
  "f(*x(y))");

  ("chained commands",
   [E_MethodCall(ast_id "a",[E_MethodCall(ast_id "b", [ast_id "c"],None,dp)],
                 None,dp)],
   "a b c"
  );

  ("chained commands with arg",
   [E_MethodCall(ast_id "a",[E_MethodCall(ast_id "b", [ast_id "c";ast_num 4],None,dp)],
                 None,dp)],
   "a b c, 4"
  );

  ("array ref method",
   [E_Binop(ast_id "x",Op_ASSIGN,E_Array([],dp),dp);
    E_MethodCall(E_Binop(ast_id "x",Op_DOT,E_Operator(Op_AREF,dp),dp),
		 [ast_num (-1);ast_num 1],
		 None,dp)],
   "x=[];x[-1,1]");

  ("array set method",
   [E_Binop(E_MethodCall(E_Binop(ast_id "x",Op_DOT,E_Operator(Op_AREF,dp),dp),
			 [ast_num (-1);ast_num 1],
			 None,dp),
	    Op_ASSIGN,
	    ast_num 3,dp)],
   "x[-1,1] = 3");

  ("nested []",
   [E_Array([
     E_MethodCall(
       E_Binop(sing_str "a",Op_DOT,E_Operator(Op_AREF,dp),dp),
       [sing_str "b"],
       None,dp)],dp)],
   "['a' ['b']]");

  ("undef as method name",
   [E_ClassDef(ast_id "A",None, 
               just_body [E_MethodDef((ast_id "undef"),[], just_body [],dp)],
               dp);
    E_MethodCall(
      E_Binop(
        E_MethodCall(E_Binop(ast_id "A",Op_DOT,ast_id "new",dp),[],None,dp),
        Op_DOT,
        ast_id "undef",dp),
      [],None,dp);
   ],
   "class A;def undef() end;end;A.new.undef()"
  );

  ("nil as method name",
   [E_ClassDef(ast_id "A",None, 
               just_body [E_MethodDef(E_Literal(Lit_Nil,dp),[], just_body [],dp)],
               dp);
    E_MethodCall(
      E_Binop(
        E_MethodCall(E_Binop(ast_id "A",Op_DOT,ast_id "new",dp),[],None,dp),
        Op_DOT,
        E_Literal(Lit_Nil,dp),dp),
      [],None,dp);
   ],
   "class A;def nil() end;end;A.new.nil()"
  );
]

let suite = "Object Parse Tests" >:::
  List.map simple_test tests
