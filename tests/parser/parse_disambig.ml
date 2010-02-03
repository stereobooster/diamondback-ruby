

open Ast
open Test_helper
open OUnit
open Printf

let mk_space_tests str uop binop = [
  (str ^ " no space before or after",
   [E_MethodDef(ast_id "x",[], just_body [ast_num 6], dp);
    E_Binop(ast_id "x",binop,ast_num 4,dp)],
   sprintf "def x() 6 end;x%s4" str
  );

  (str ^ " space before and after in mid expr",
   [E_MethodDef(ast_id "x",[], just_body [ast_num 6], dp);
    E_Binop(ast_id "x",binop,ast_num 4,dp)],
   sprintf "def x() 6 end;x %s 4" str
  );

  (str ^ " space before nl after in mid expr",
   [E_MethodDef(ast_id "x",[], just_body [ast_num 6], dp);
    E_Binop(ast_id "x",binop,ast_num 4,dp)],
   sprintf "def x() 6 end;x %s\n4" str
  );

  (str ^ " space before and after in beg expr",
   [E_MethodDef(ast_id "x",[], just_body [ast_num 6], dp);
    E_Unary(uop,ast_num 4,dp)],
   sprintf "def x() 6 end; %s 4" str
  );

  (str ^ " space before, not after",
   [E_MethodDef(ast_id "x",[], just_body [ast_num 6], dp);
    E_MethodCall(ast_id "x",[E_Unary(uop,E_Block([ast_num 4],dp),dp)],None,dp)],
   sprintf "def x() 6 end;x %s(4)" str
  );

]

let tests = [  
  ("+e after literal is binop",
   [E_Binop(E_Binop(ast_num 1,Op_PLUS,ast_num 2,dp),Op_PLUS,ast_num 3,dp)],
   "1 + 2 +3"
  );

  ("-@ is binary minus ivar",
   [E_Binop(ast_id "@x",Op_ASSIGN,ast_num 3,dp);
    E_Binop(ast_id "y",Op_ASSIGN,ast_num 2,dp);
    E_Binop(ast_id "y",Op_MINUS,ast_id "@x",dp);
   ],
   "@x = 3;y = 2;y-@x"
  );

  ("-@ is unary minus",
   [E_MethodDef(E_UOperator(Op_UMinus,dp),[Formal_id (ast_id "w")], 
                just_body [ast_num 3], dp);
    E_Binop(ast_id "x",Op_ASSIGN, ast_num 4,dp);
    E_MethodCall(E_Binop(E_Literal(Lit_Self,dp),Op_DOT,E_UOperator(Op_UMinus,dp),dp),
                 [ast_id "x"], None, dp)
    ],
   "def -@(w) 3 end;x = 4;self.-@x"
  );

  ("negative literal is not parsed as uop",
   [E_MethodCall(E_Binop(ast_num (-18),Op_DOT,ast_id "to_s",dp),[],None,dp)],
   "-18.to_s"
  );

  ("positive literal is not parsed as uop",
   [E_MethodCall(E_Binop(ast_num 18,Op_DOT,ast_id "to_s",dp),[],None,dp)],
   "+18.to_s"
  );

  ("def paren starts expression",
   [E_MethodDef(ast_id "f",[],just_body [ast_atom "hi"],dp)],
   "def f() :hi end"
  );

  ("/ is regexp start",
   [E_MethodCall(ast_id "f",[ast_re "12$" ""],None,dp)],
   "f /12$/"
  );

  ("cmd / arg is divide",
   [E_Binop(ast_id "f",Op_DIV,ast_num 2,dp)],
   "f / 2"
  );

  ("local pre space / is divide",
   [E_Binop(ast_id "x",Op_ASSIGN,ast_num 1,dp);
    E_Binop(ast_id "x",Op_DIV,ast_num 3,dp)],
   "x=1;x /3"
  );

  ("meth pre space / is regexp",
   [E_MethodCall(ast_id "x",[ast_re "3" "i"],None,dp)],
   "x /3/i"
  );

  ("e.f / no space is divide",
   [E_Binop(E_MethodCall(E_Binop(ast_num 3,Op_DOT,ast_id "succ",dp),[],None,dp),
            Op_DIV, ast_num 2, dp)],
   "3.succ/2"
  );

  ("e.f / pre space is regexp",
   [E_MethodCall(E_Binop(ast_id "e",Op_DOT,ast_id "f",dp),
                 [ast_re "2" "i"],None,dp)],
   "e.f /2/i"
  );

  ("e.f./ is divide",
   [E_MethodCall(
      E_Binop(
        E_MethodCall(E_Binop(ast_id "e",Op_DOT,ast_id "f",dp),[],None,dp),
        Op_DOT,
        E_Operator(Op_DIV,dp),dp),
      [ast_num 2],None,dp)],
   "e.f./2"
  );

  ("e /= is assign",
   [E_Binop(ast_id "w", Op_OP_ASGN Op_DIV,ast_num 2,dp)],
   "w /= 2"
  );

  ("f(/=/) is regexp",
   [E_MethodCall(ast_id "f", [ast_re "=" ""], None, dp)],
   "f(/=/)"
  );

  ("cmd[]",
   [E_MethodCall(E_Binop(ast_id "x",Op_DOT,E_Operator(Op_AREF,dp),dp),
                [ast_num 1], None, dp)],
   "x[1]"
  );

  ("cmd []",
   [E_MethodCall(ast_id "x", [E_Array([ast_num 1],dp)],None,dp)],
   "x [1]"
  );

  ("* no space before or after",
   [E_MethodDef(ast_id "x",[], just_body [ast_num 6], dp);
    E_Binop(ast_id "x",Op_TIMES,ast_num 4,dp)],
   "def x() 6 end;x*4"
  );

  ("* space before and after in mid expr",
   [E_MethodDef(ast_id "x",[], just_body [ast_num 6], dp);
    E_Binop(ast_id "x",Op_TIMES,ast_num 4,dp)],
   "def x() 6 end;x * 4"
  );

  ("def binop *",
   [E_MethodDef(E_Operator(Op_TIMES,dp),[Formal_id (ast_id "x")],
                just_body [ast_id "x"], dp)],
   "def *(x) x end"
  );

  ("splat space before, not after",
   [E_MethodDef(ast_id "x",[], just_body [ast_num 6], dp);
    E_MethodCall(ast_id "x",[E_Unary(Op_UStar,ast_num 4,dp)],None,dp)],
   "def x() 6 end;x *4"
  );

  ("& no space before or after",
   [E_MethodDef(ast_id "x",[], just_body [ast_num 6], dp);
    E_Binop(ast_id "x",Op_BAND,ast_num 4,dp)],
   "def x() 6 end;x&4"
  );

  ("& space before and after in mid expr",
   [E_MethodDef(ast_id "x",[], just_body [ast_num 6], dp);
    E_Binop(ast_id "x",Op_BAND,ast_num 4,dp)],
   "def x() 6 end;x & 4"
  );

  ("& space before, not after",
   [E_MethodDef(ast_id "x",[], just_body [ast_num 6], dp);
    E_MethodCall(ast_id "x",[E_Unary(Op_UAmper,ast_id "y",dp)],None,dp)],
   "def x() 6 end;x &y"
  );

  ("cmd ?x is char code",
   [E_MethodCall(ast_id "f", [ast_num (Char.code 'x')], None, dp)],
   "f ?x"
  );

  ("e.f ?x is char code",
   [E_MethodCall(
      E_Binop(ast_id "e", Op_DOT, ast_id "f",dp),
      [ast_num (Char.code 'x')], None, dp)],
   "e.f ?x"
  );

  ("()? is tern",
   [E_Ternary(E_Block([ast_num 0],dp), ast_num 1,ast_num 2,dp)],
   "(0)?1:2"
  );

  ("x?\n is tern",
   [E_Ternary(ast_id "@x", ast_num 1,ast_num 2,dp)],
   "@x ?\n1:2"
  );

  ("? starts expr beg",
   [E_Ternary(ast_num 3, E_Array([sing_str "."], dp),ast_num 2,dp)],
   "3 ? ['.'] : 2"
  );

  ("def then array",
   [E_MethodDef(ast_id "f",[],just_body [E_Array([ast_id "a";ast_id "b"],dp)],dp)],
   "def f() [a,b] end"
  );

  ("% is string",
   [E_Binop(
      E_Binop(E_Block([E_Binop(ast_id "n",Op_PLUS,ast_num 1,dp)],dp),
              Op_REM, ast_id "N",dp),
      Op_TIMES, ast_num 2,dp)],
   "(n+1)%N*2"
  );

  ("%()[ is arg",
   [E_MethodCall(
      E_Binop(doub_str "a",Op_DOT,E_Operator(Op_AREF,dp),dp),
      [ast_num 0],None,dp)],
   "%(a)[0]"
  );

  ("e.f % is %str",
   [E_MethodCall(E_Binop(ast_id "e", Op_DOT, ast_id "f",dp),
                 [doub_str "x"],None,dp)],
   "e.f %{x}"
  );

  ("spc %{x}",
   [doub_str "S"],
   " %(S)"
  );

  ("local << is shift",
   [E_Binop(ast_id "x",Op_ASSIGN,ast_num 1,dp);
    E_Binop(ast_id "x",Op_LSHIFT,ast_id "y",dp)],
   "x=1;x << y"
  );

  ("cmd << space is shift",
   [E_Binop(ast_id "f",Op_LSHIFT,ast_id "y",dp)],
   "f << y"
  );

  ("cmd << no-space is heredoc",
   [E_MethodCall(ast_id "f",[doub_str ""],None,dp)],
   "f <<y\ny"
  );

  ("e.f << no-space is heredoc",
   [E_MethodCall(
      E_Binop(ast_id "e",Op_DOT,ast_id "f",dp),
      [doub_str ""],None,dp)],
   "e.f <<y\ny"
  );

  ("m (2,3) is a two-arg method call",
   [E_MethodCall(ast_id "m",[ast_num 2;ast_num 3],None,dp)],
   "m (2,3)"
  );

  ("m (2+3),4 is a two-arg method call",
   [E_MethodCall(ast_id "m",[E_Block([E_Binop(ast_num 2,Op_PLUS,ast_num 3,dp)],dp);
                             ast_num 4],
                 None,dp)],
   "m (2+3),4"
  );
     
] @ mk_space_tests "-" Op_UMinus Op_MINUS 
  @ mk_space_tests "+" Op_UPlus Op_PLUS


let ignores = [
]

let suite = "Disambiguation Tests" >:::
  List.map simple_test tests @ List.map simple_test_ignore ignores
