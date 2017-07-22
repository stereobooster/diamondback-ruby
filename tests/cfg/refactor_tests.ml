
open OUnit

open Cfg_test_helper
open Cfg
module C = Abbr

let id x =
  let len = String.length x in
    if len = 0 then assert false;
    let kind = match x.[0] with
      | 'a'..'z' | '_' -> `Var_Local
      | '@' -> if x.[1] = '@' then `Var_Class else `Var_Instance
      | '$' -> `Var_Global
      | 'A'..'Z' -> `Var_Constant
      | _ -> raise (Invalid_argument "id")
    in
      `ID_Var(kind, x)
	  
let fvar x = id (Printf.sprintf "__tmp_%d" x)
let num i = `Lit_FixNum i
let str s = `Lit_String s

let mk s = mkstmt s dp

let nil = mk (Expression `ID_Nil)

let defm args body =
  mk (Method(Instance_Method(`ID_MethodName "m"), args, body))

let mc ret targ msg args = 
  mk (MethodCall(ret,
		{mc_target=targ;
		 mc_msg=msg;
		 mc_args = args;
		 mc_cb = None}))

let mc_n ?targ msg = mc None targ (`ID_MethodName msg)
let mc_s s ?targ msg = mc (Some s) targ (`ID_MethodName msg)

	 
let tests = [  
    ("hash",
    mk (Expression (`Lit_Hash [ (num 1, num 2) ])),
    "{1 => 2}"
    );

    ("hash list",
    mk (Expression (`Lit_Hash [ (num 1, num 2) ])),
    "{1, 2}"
    );

    ("nested hash",
    mk (Expression 
	   (`Lit_Hash [
	       (`Lit_Hash [
		   (num 1), 
		   (num 2)
		 ]),
	       (num 3)])
       ),
    "{{1 => 2} => 3}"
    );

    ("range inc",
    mk (Expression (`Lit_Range(true,`Lit_FixNum 1, `Lit_FixNum 5))),
    "1...5");

    ("range exc",
    mk (Expression (`Lit_Range(false,`Lit_FixNum 1, `Lit_FixNum 5))),
    "1..5");

    ("range assign",
     mk (Assign(id "x",`Lit_Range(false,num 1,num 2))),
     "x = 1..2"
    );

    ("nested assign",
    mk (Seq [
	mk (Assign(id "y",num 2));
	mk (Assign(id "x",id "y"));
      ]),
    "x = y = 2");

    ("defined",
    mk (Defined(fvar 1, mk (Expression(id "@x")))),
    "defined?(@x)");

    ("if defined",
     mk (Seq [
	   mk (Defined(fvar 1, mk (Expression(id "@x"))));
	   mk (If(fvar 1, mk (Expression(id "@x")), mk (Expression(id "@y"))));
	 ]),
    "if defined?(@x); @x else @y end");

    ("proc method arg",
     mk (Seq [
	   mk (Assign(id "x", id "@y"));
           C.call "f" [] ~cb:(CB_Arg (id "x")) dp;
	 ]
	),
    "x = @y;f(&x)"
    );

    ("assign",
     mk (Assign(id "f",id "@x")),
    "f = @x");

    ("assign lone id methodcall",
     mc_s (id "f") "x" [],
    "f = x");
    
    ("nested method",
     mk (Seq [
	   mc_s (fvar 1) "f" [];
	   mc_n ~targ:(fvar 1) "x" [];
	 ]),
    "f.x");
    
    ("assign method",
     mk
       (Seq[
	   mc_s (id "x") ~targ:(id "A") "new" [];
	   mk (MethodCall(None,
			 {mc_target=Some (id "x");
			  mc_msg=`ID_Assign "y";
			  mc_args=[`Lit_FixNum 2];
			  mc_cb = None})
	     );
	]),
    "x = A.new; x.y = 2");

    ("binop assign doesn't intro tmp",
     mk (MethodCall(Some (id "x"),
		    {mc_target = Some (id "@y");
		     mc_msg=`ID_Operator Op_Plus;
		     mc_args=[`Lit_FixNum 2];
		     mc_cb = None})),
     "x = @y + 2"
    );

    ("&& into ifs",
     mk(Seq [mk (If(id "@a", 
		     mk (Assign(fvar 1, id "@c")),
		     mk (Assign(fvar 1, id "@a"))
		     ));
	       mk (Assign(id "x", fvar 1))
	      ]
       ),
     "x = @a && @c"
    );

    ("|| into ifs",
     mk(Seq [mk (If(id "@a", 
		     mk (Assign(fvar 1, id "@a")),
		     mk (Assign(fvar 1, id "@c"))
		     ));
	       mk (Assign(id "x", fvar 1))
	      ]
       ),
     "x = @a || @c"
    );

    ("! into if",
     mk(Seq [mk (If(id "@y", 
		      (mk (Assign(fvar 1,`ID_False))),
		      (mk (Assign(fvar 1,`ID_True)))));
	       mk (Assign(id "x", fvar 1))
	      ]
       ),
     "x = ! @y"
    );

    ("!= into (not ==)",
     mk (Seq [
	   mc (Some (fvar 1)) (Some (id "@x")) (`ID_Operator Op_EQ) [(id "@y")];
	   mk (If(fvar 1,
		  mk (Assign(fvar 2,`ID_False)),
		  mk (Assign(fvar 2,`ID_True))
		 )
	      );
	   mk (Expression (fvar 2));
	 ]),
     "@x != @y"
    );

    ("if != into if (not ==)",
     mk (Seq [
	   mc (Some (fvar 1)) (Some (id "@x")) (`ID_Operator Op_EQ) [(id "@y")];
	   mk (If(fvar 1,
		  mk (Assign(fvar 2,`ID_False)),
		  mk (Assign(fvar 2,`ID_True))
		 )
	      );
	   mk (If(fvar 2,
		  mk (Expression (id "@z")),
                  nil
		 )
	      );
	 ]),
     "if @x != @y then @z end"
    );

    ("!~ into (not =~)",
     mk (Seq [
	   mc (Some (fvar 1)) (Some (id "@x")) (`ID_Operator Op_Match) [(id "@y")];
	   mk (If(fvar 1,
		  mk (Assign(fvar 2,`ID_False)),
		  mk (Assign(fvar 2,`ID_True))
		 )
	      );
	   mk (Expression (fvar 2));
	 ]),
     "@x !~ @y"
    );

    ("seq assign",
     mk (Seq [
	   mk (MethodCall(None,{mc_target=None;mc_msg=`ID_MethodName "x";
				mc_args=[];mc_cb = None}));
	   mk (Assign(id "z",id "@y"))
	 ]),
     "z = (x();@y)"
    );

    ("expr becomes return",
    defm [] (mk (Return (Some (id "@x")))),
    "def m() @x end"
    );

    ("expr seq becomes return",
    defm [] 
      (mk (Seq [
	  mk (Expression (id "@x"));
	  mk (Expression (id "@y"));
	  mk (Return (Some (id "@z")));
	]
	  )
      ),
    "def m() @x;@y;@z end"
    );


    ("assign becomes return",
    defm []
      (mk (Seq [
	  mk (Assign(id "x", id "@y"));
	  mk (Return (Some (id "x")));
	]
	  )
      ),
    "def m() x = @y end"
    );

    ("multi-assign becomes return",
    defm []
      (mk (Seq [
	  mk (Assign(`Tuple [id "a";id "b"; id "c"], id "@y"));
	  mk (Return (Some (`Tuple [id "a";id "b"; id "c"])));
	]
	  )
      ),
    "def m() a,b,c = @y end"
    );

    ("yield becomes return",
    defm []
      (mk (Seq [
	  mk (Yield(Some (fvar 1), []));
	  mk (Return (Some (fvar 1)));
	]
	  )
      ),
    "def m() yield() end"
    );
    
    ("assign yield becomes return",
    defm []
      (mk (Seq [
	  mk (Yield(Some (id "y"), []));
	  mk (Return (Some (id "y")));
	]
	  )
      ),
    "def m() y = yield() end"
    );

    ("methcall becomes return",
    defm []
      (mk (Seq [
	  mc_s (fvar 1) ~targ:(id "@x") "y" [];
	  mk (Return (Some (fvar 1)));
	]
	  )
      ),
    "def m() @x.y() end"
    );

    ("assign methcall becomes return",
    defm []
      (mk (Seq [
	  mc_s (id "z") ~targ:(id "@x") "y" [];
	  mk (Return (Some (id "z")));
	]
	  )
      ),
    "def m() z = @x.y() end"
    );

    ("return stays return",
    defm [] (mk (Return (Some (id "@y")))),
    "def m() return @y end"
    );

    ("if nests return",
    defm []
      (mk (If(`ID_True,
	     mk (Return (Some (id "@x"))),
	     mk (Return (Some (id "@y")))))
      ),
    "def m() if true; @x else @y end end"
    );

    ("While doesn't nest return",
    defm [] 
      (mk (Seq [
	  mk (While(`ID_True,
		   mk (Expression (id "@y"))
		   ));
	  mk (Return (Some `ID_Nil))
	])
      ),
    "def m() while true; @y end end"
    );

    ("while cmd do/end",
     mk (While(`ID_True,
               mk (Seq [
                     mc_s (fvar 1) "x" [];
                     mk (If(fvar 1, mk (Expression (id "@y")),
                           mk (Break None)))
                   ]))),
     "while x do @y end"
    );

    ("while cmd {blk}",
     mk (While(`ID_True,
               mk (Seq [
                     C.call ~lhs:(fvar 1) "x" []
                       ~cb:(CB_Block([],mk (Next (Some (id "@y"))))) dp;
                     mk (If(fvar 1, mk (Expression (id "@z")),
                           mk (Break None)))
                   ]))
	),
     "while x {|| @y} do @z end"
    );

    ("do/while",
     mk(While(`ID_True,
              mk (Seq [
                    mk (Assign(id "x",num 1));
                    mk (If(`ID_True,mk (Next None), mk (Break None)));
                  ]))
       ),
     "begin x = 1 end while true"
    );

    ("() not do/while",
     mk(While(`ID_True,mk (Assign(id "x",num 1)))),
     "(begin x = 1 end) while true"
    );

    ("for returns guard",
    defm [] 
      (mk (Seq [
	  mc_s (fvar 1) ~targ:(id "@f") "g" [];
	  mk (For([`Formal_block_id(`Var_Local, "x")],
		 (fvar 1),
		 mk (Expression (id "@z"))));
	  mk (Return (Some (fvar 1)));
	])
      ),
    "def m() for x in @f.g() do; @z end end"
    );

    ("for binds locals",
    mk (For([`Formal_block_id(`Var_Local, "x")],
	   (id "@y"),
	   mk (Expression (id "x")))
       ),
     "for x in @y do; x end");
	     
    ("case nests return",
    defm []
      (mk (Case {
	  case_guard = (id "@a");
	  case_whens = [(id "B"), mk (Return (Some (id "@c")))];
	  case_else = Some(mk (Return (Some (id "@d"))));
	   })
      ),
     "def m(); case @a;when B; @c; else; @d; end end"
    );

(*
    ("begin/end nests return",
    mk (Method(Instance_Method(`ID_MethodName "m"),
	      [],
	      mk (ExnBlock {
		  exn_body = mk (Expression (id "x"));
		  exn_rescue = [];
		  exn_ensure = None;
		  exn_else = Some (mk (Expression (id "y")));
		})
	      )
       ),
    "def m() begin 5 ensure 6 else 7 end end"
    );
*)

    ("begin else end",
    mk (ExnBlock {
	exn_body = mk (Expression (id "@x"));
	exn_rescue = [];
	exn_ensure = None;
	exn_else = Some (mk (Expression (id "@y")));
      }),
    "begin @x; else @y end"
    );

    ("for guard hoists",
    mk (Seq [
	mc_s (fvar 1) ~targ:(id "@f") "g" [];
	mk (For([`Formal_block_id(`Var_Local, "x")],
	       (fvar 1),
	       mk (Expression (id "@z"))));

      ]),
    "for x in @f.g() do; @z end"
    );

    ("formal default",
    (let def = `Lit_Atom("__rat_default_1") in
    let fcall = mc_s (id "x") "f" [] in
    let eqcall = mc_s (fvar 1) ~targ:(`ID_Var(`Var_Local,"x")) "eql?" [def] in
      defm [`Formal_default("x", def)]
	(mk (Seq [
	    eqcall;
	    mk (If((fvar 1), fcall, nil));
	    mk (Return (Some `ID_Nil));
	  ]
	    )
	)
    ),
    "def m(x = f()); end"
    );

    ("lone id is a methodcall",
    defm [] (
      mk (Seq [mc_s (fvar 1) "x" [];
		 mk (Return (Some (fvar 1)))];
	 )
    ),
    "def m(); x end"
    );

    ("multi-assign doesn't translate locals into mc",
     defm []
       (mk (Seq [
	      mk (Assign(`Tuple [id "a";id "b"; id "c"], id "@y"));
	      mk (Expression (id "a"));
	      mk (Expression (id "b"));
	      mk (Return (Some (id "c")));
	    ]
	   )
       ),
     "def m() a,b,c = @y; a; b; c end"
    );

    ("star-lhs doesn't translate into mc",
     defm []
       (mk (Seq [
	      mk (Assign(`Star (id "y"), `Lit_Array []));
	      mc_s (fvar 1) ~targ:(id "y") "length" [];
	      mk (Return (Some (fvar 1)));
	    ])
       ),
     "def m(); *y = []; y.length end"
    );

    ("formal args don't transalte into mc",
    defm [`Formal_meth_id "x"]
      (mk (Return (Some (id "x")))),
    "def m(x); x end"
    );

    ("rescue binding doesn't xlate into mc",
      (mk
	  (ExnBlock {
	      exn_body = mk (Expression (num 1));
	      exn_rescue = 
	      [
		{ rescue_guards = [Rescue_Bind(id "Object", id "y")];
		  rescue_body = mk (Expression (id "y"));
		}
	      ];
	      exn_ensure = None;
	      exn_else = None;
	    })
      ),
    "begin\n  1\nrescue Object => y\n  y\nend"
    );

    ("anonymous rescue binding doesn't xlate into mc",
      (mk
	  (ExnBlock {
	      exn_body = mk (Expression (num 1));
	      exn_rescue = 
	      [
		{ rescue_guards = [Rescue_Bind(id "StandardError", id "y")];
		  rescue_body = mk (Expression (id "y"));
		}
	      ];
	      exn_ensure = None;
	      exn_else = None;
	    })
      ),
    "begin\n  1\nrescue => y\n  y\nend"
    );

    ("rescue w/o binding",
      (mk
	  (ExnBlock {
	      exn_body = mk (Expression (num 1));
	      exn_rescue = 
	      [
		{ rescue_guards = [Rescue_Expr(id "Fixnum")];
		  rescue_body = mk (Expression (num 2));
		}
	      ];
	      exn_ensure = None;
	      exn_else = None;
	    })
      ),
    "begin\n  1\nrescue Fixnum\n  2\nend"
    );

    ("return compound",
    defm []
      (mk (Seq [
	  mc_s (fvar 3) ~targ:(id "@x") "f" [];
	  mk (MethodCall(Some (fvar 2),
			{mc_target=Some (fvar 3);
			 mc_msg=`ID_Operator Op_Plus;
			 mc_args = [num 3];
			 mc_cb = None}));

	  mk (MethodCall(Some (fvar 1),
			{mc_target=Some (fvar 2);
			 mc_msg=`ID_Operator Op_Div;
			 mc_args = [num 2];
			 mc_cb = None}));
	  mk (Return(Some (fvar 1)));
	])
      ),
    "def m(); return (@x.f+3)/2 end"
    );
    
    ("||=",
     mk (If(id "@x",mk (Expression (id "@x")), mk (Assign(id "@x", id "@y")))),
     "@x ||= @y"
    );

    ("&&=",
     mk (If(id "@x", mk (Assign(id "@x", id "@y")),mk (Expression (id "@x")))),
     "@x &&= @y"
    );

    ("||= fresh lhs",
     mk (Assign(id "x",num 1)),
     "x ||= 1"
    );

    ("assign under ||=",
     mk (Seq [
	   mk (Assign(id "x",num 1));
	   mk (If(id "x",mk (Expression (id "x")), mk (Assign(id "x", id "@y"))))
	 ]),
     "x=1;x ||= @y"
    );

    ("default argt",
    defm [`Formal_default("x",num 1)] (mk (Return (Some (id "x")))),
    "def m(x = 1) x end"
    );

    ("* in yield is not uop",
    defm [`Formal_meth_id "key"; `Formal_star "sw"]
      (mk (Seq [
	  mk (Yield(Some(fvar 1), [id "key"; `Star(id "sw")]));
	  mk (Return(Some (fvar 1)));
	])),
    "def m(key,*sw); yield(key, *sw); end"
    );

    ("+= in expr",
     mk (Seq [
           mc (Some (fvar 1)) (Some (id "@x")) (`ID_Operator Op_Plus) [num 1];
	   mk (Assign(id "@x", fvar 1)); (* TODO elim extra assignments *)
           mk (Assign(fvar 2, id "@x"));
	   mk (Assign(id "y", fvar 2));
	 ]),
     "y = (@x += 1)"
    );

    ("+= with []=",
     mk (Seq [
           mc (Some (fvar 2)) (Some (id "@x")) (`ID_Operator Op_ARef) [num 0];
           mc (Some (fvar 1)) (Some (fvar 2)) (`ID_Operator Op_Plus) [num 1];
           mc None (Some (id "@x")) (`ID_Operator Op_ASet) [num 0; fvar 1]
         ]),
     "@x[0] += 1"
    );

   ("local intro in block",
   mk (Seq [
         C.call "x" [] 
           ~cb:(CB_Block([],mk (Seq [
				  mk (Assign(id "y", num 2));
				  mk (Next(Some(id "y")));
				]))) dp;
         mc_n "y" [];
     ]),   
   "x() {|| y = 2}; y"
   );

   ("formal intro in block",
    C.call "x" [] ~cb:(CB_Block([`Formal_block_id(`Var_Local, "y")],
				mk (Next(Some (id "y"))))) dp,
    "x() {|y| y}"
   );

   ("formal intro in block scoped",
   mk (Seq [
         C.call "x" []
           ~cb:(CB_Block([`Formal_block_id(`Var_Local, "y")],
			 mk (Seq [
			       mk (Assign(id "y", num 2));
			       mk (Next(Some(id "y")));
			     ]))) dp;
       mc_n "y" [];
     ]),   
   "x() {|y| y = 2}; y"
   );

   ("super",
    defm [] (C.seq [mc (Some (fvar 1)) None `ID_Super [];
                    C.return ~v:(fvar 1) dp] dp;
            ),
    "def m(); super; end"
   );

   ("super mc",
    mc_n ~targ:(id "@x") "super" [],
    "@x.super"
   );

   ("break",
    mk (Break None),
    "break"
   );

   ("break args",
    mk (Break(Some (`Tuple [num 1; num 2]))),
    "break 1,2"
   );

   ("redo",
    mk Redo,
    "redo"
   );

   ("retry",
    mk Retry,
    "retry"
   );

   ("next",
    mk (Next None),
    "next"
   );

   ("next arg",
    mk (Next(Some (`Tuple [num 1; num 2]))),
    "next 1,2"
   );

   ("undef",
    mk (Undef [`ID_MethodName "x"]),
    "undef x"
   );

   ("heredoc",
   mk (Expression (`Lit_String "test\n")),
   "<<DELIM
test
DELIM");

   ("interp string",
   mk (Seq [
       mc_s (fvar 1) ~targ:(id "@b") "to_s" [];
       mc (Some (fvar 2)) (Some (str "hi ")) (`ID_Operator Op_Plus) [fvar 1];
       mk (Assign((id "x"), (fvar 2)));
     ]),
   "x = \"hi #{@b}\"");

   ("interp atom",
   mk (Seq [
       mc_s (fvar 1) ~targ:(id "@y") "to_s" [];
       mc (Some (fvar 2)) (Some (str "x")) (`ID_Operator Op_Plus) [fvar 1];
       mc_s (fvar 3) ~targ:(fvar 2) "to_sym" [];
       mk (Expression (fvar 3));
     ]),
   ":\"x#{@y}\"");

   ("x[y] becomes x.[](y)",
    mc None (Some (id "@x")) (`ID_Operator Op_ARef) [id "@y"],
    "@x[@y]"
   );

   ("x[y] = z becomes x.[]=(y,z)",
    mc None (Some (id "@x")) (`ID_Operator Op_ASet) [id "@y"; id "@z"],
    "@x[@y] = @z"
   );

   ("@x[1] = f() becaomes tmp=f();@x.[]=(1,tmp)",
    mk (Seq [
	  mc_s (fvar 1) "f" [];
	  mc None (Some (id "@x")) (`ID_Operator Op_ASet) [num 1; fvar 1];
	]),
    "@x[1] = f()"
   );

  ("escaped # in string",
   mk (Expression (str "\\#{hi}")),
   "\"\\#{hi}\""
  );

  ("@x.y = @w.z is @x.y=(@w.z)",
   mk (Seq [
	 mc_s (fvar 1) ~targ:(id "@w") "z" [];
	 mc None (Some (id "@x")) (`ID_Assign "y") [(fvar 1)];
       ]),
   "@x.y = @w.z"
  );

  ("@x,y[0,1] = 2,3 has @y.[]=(0,1,3)",
   mk (Seq [
	 mk (Assign((`Tuple[id "@x";fvar 1]), (`Tuple [num 2; num 3])));
	 mc None (Some (id "@y")) (`ID_Operator Op_ASet) [num 0; num 1;fvar 1];
       ]),
   "@x,@y[0,1] = 2,3"
  );

  ("*when",
   (mk (Seq[
	  mk (Assign(id "x", `ID_True));
	  mk (Case {
		case_guard = `ID_True;
		case_whens = [`Star(id "x"), mk (Expression `ID_False)];
		case_else = None;
	      });
	])
   ),
   "x=true;case true when *x; false; end"
  );

  ("methodcall with ::",
   mc_n ~targ:(id "A") "m" [],
   "A::m"
  );

  ("__LINE__",
   (mk (Expression (`Lit_FixNum 1))),
   "__LINE__"
  );     

  ("__FILE__",
   (* should return the actual file, since its a string (not a file),
      it returns the string itself *)
   (mk (Expression (str "__FILE__"))),
   "__FILE__"
  );     

  ("meta-class",
   (mk (Seq [
	  mk (Assign(id "x", str "s"));
	  mk (Class(None,MetaClass(id "x"), 
		    mk (Method(Instance_Method(`ID_MethodName "m"),[],
			       mk (Return (Some `ID_Nil)))))
	     );
	])
   ),
   "x='s'\nclass << x\n def m() end end"
  );

  ("alias symbols",
   (mk (Alias(Alias_Method(`ID_MethodName "x", `ID_MethodName "y")))),
   "alias :x :y"
  );

  ("alias bin-op",
   (mk (Alias(Alias_Method(`ID_Operator Op_Plus, `ID_Operator Op_Minus)))),
   "alias + -"
  );

  ("alias bin-op symbols",
   (mk (Alias(Alias_Method(`ID_Operator Op_Plus, `ID_Operator Op_Minus)))),
   "alias :+ :-"
  );

  ("alias u-op",
   (mk (Alias(Alias_Method(`ID_UOperator Op_UPlus, `ID_UOperator Op_UTilde)))),
   "alias +@ ~"
  );

  ("alias u-op symbols",
   (mk (Alias(Alias_Method(`ID_UOperator Op_UPlus, `ID_UOperator Op_UTilde)))),
   "alias :+@ :~"
  );

  ("alias op/symbol",
   (mk (Alias(Alias_Method(`ID_MethodName "x", `ID_Operator Op_Plus)))),
   "alias :x +"
  );

  ("alias globals",
   (mk (Alias(Alias_Global(`ID_Var(`Var_Global,"$x"),`ID_Var(`Var_Global,"$y"))))),
   "alias $x $y"
  );

  ("alias global/builtin",
   (mk (Alias(Alias_Global(`ID_Var(`Var_Global,"$x"),`ID_Var(`Var_Builtin,"$2"))))),
   "alias $x $2"
  );

  ("resolve methcall inside defined?",
   (mk (Seq [mk (Assign(id "x", num 1));
	     mk (Defined(fvar 1, mc_n ~targ:(id "x") "y" []));
	     mk (If(fvar 1, mk (Expression (num 2)), mk (Expression (num 3))))
	    ])),
   "x=1;if defined? x.y then 2 else 3 end"
  );

  ("return super",
    defm []
      (mk (Seq [
	     mc (Some (fvar 1)) None `ID_Super [num 1];
	     mk (Return(Some (fvar 1)));
	   ])),
   "def m(); return super(1) end"
  );

  ("interp in heredoc",
   mk (Seq [
       mc_s (fvar 1) ~targ:(id "@b") "to_s" [];
       mc (Some (fvar 2)) (Some (str "hi ")) (`ID_Operator Op_Plus) [fvar 1];
       mc (Some (fvar 3)) (Some (fvar 2)) (`ID_Operator Op_Plus) [str "\n"];
       mk (Assign((id "x"), (fvar 3)));
     ]),
   "x = <<EOF
hi #{@b}
EOF");

  ("exn ensure no return",
   defm []
     (mk
        (ExnBlock {
	   exn_body = mk (Return (Some (num 1)));
	   exn_rescue = [];
	   exn_ensure = Some(mk (Expression `ID_False));
	   exn_else = None;
         })
     ),
   "def m() begin 1 ensure false end end"
  );

(*
  ("seen under ||=",
    defm [`Formal_meth_id "y"]
      (mk (Seq [
	     mk (If(id "@x",
		    nil, 
		    mk (Assign(id "@x", id "@y"))
		   ));
	     (*mc (Some (fvar 1)) `No_Targ `ID_Super [num 1];*)
	     mk (Return(Some (fvar 1)));
	   ])),

   "def m(y) x ||= y end"
  );
*)

(* x[3] ||= 4 *)

  ("packed multi arg assignemnt method",
   mc None (Some (id "@x")) (`ID_Assign "m") [`Lit_Array([num 2; num 3])],
   "@x.m = 2,3"
  );
   

  ("class body assign outer",
   mk (Seq [
         mk (Class(Some (fvar 1),NominalClass(id "A",None),
	           mk (Expression (num 0))));
         mk (Assign(id "x", fvar 1))
       ]),
   "x = class A; 0; end"
  );

  ("module body assign outer",
   mk (Seq [
         mk (Module(Some (fvar 1),id "A",
	           mk (Expression (num 0))));
         mk (Assign(id "x", fvar 1))
       ]),
   "x = module A; 0; end"
  );

  ("&nil argument",
   C.call "f" [] ~cb:(CB_Arg `ID_Nil) dp,
   "f(&nil)"
  );

  ("nil def target",
   mk (Method(Singleton_Method(`ID_Nil,`ID_MethodName "f"),
	      [], mk (Return(Some `ID_Nil)))),
   "def nil.f() end"
  );

  ("undef symbol",
   mk (Undef [`ID_MethodName "to_s"]),
   "undef :to_s"
  );

  ("star arg in ASet",
   mk (Seq [
         mc (Some (fvar 1)) (Some (`Lit_Array [`Star (id "@y")]))
           (`ID_Operator Op_Plus) [`Lit_Array[id "@z"]];
         mc None (Some (id "@x")) (`ID_Operator Op_ASet) [`Star (fvar 1)]
       ]),
   "@x[*@y] = @z"
  );

  ("seen local in rescue",
   mk (Seq [
         mk (Assign(id "x",num 1));
         mk (ExnBlock {
	       exn_body = mk (Expression `ID_True);
	       exn_rescue = [
		 { rescue_guards = [Rescue_Bind(id "x",id "e")];
		   rescue_body = mk (Expression `ID_False);
                 }];
	       exn_ensure = None;
	       exn_else = None;
             })
       ]),
   "x = 1
begin true
rescue x => e
  false
end
");

  ("regexp interp modifier",
   mk
     (Seq [
        mc_s (fvar 2) ~targ:(num 3) "to_s" [];
        mc_s (fvar 1) ~targ:(`ID_UScope "Regexp") "new"
          [fvar 2; (`ID_Scope(`ID_Var(`Var_Constant,"Regexp"),"IGNORECASE"))];
        mk (Expression(fvar 1));
      ]),
   "/#{3}/i"
  );

  ("regexp interp o-modifier",
   begin let glob = `ID_Var(`Var_Global,"$__druby_global_1_1") in
     mk
       (Seq [
          mk (If(glob,
                 mk (Expression glob),
                 mk (Seq [mc_s (fvar 2) ~targ:(num 3) "to_s" [];
                          mc_s glob ~targ:(`ID_UScope "Regexp") "new" [fvar 2; num 0];
                         ])));
          mk (Expression glob);
        ])
   end,
   "/#{3}/o"
  );

  ("assignable method def",
   mk (Method(Instance_Method(`ID_Assign "foo"), [`Formal_meth_id "x"],
              mk (Seq [mk (Assign (id "@foo",id "x"));
                       mk (Return (Some (id "@foo")))]))),
   "def foo=(x) @foo = x end"
  );

  ("type annot",
   begin let stmt = 
     mk (Class(None,NominalClass(id "A",None),empty_stmt ()))
   in 
   let annot = Annotation.ClassType("A",[Annotation.QVar "t",None],[]) in
     add_annotation stmt annot
   end,
   "##% A<t>\nclass A; end"
  );

  ("sym to proc",
   C.call "f" [] ~cb:(CB_Arg (`Lit_Atom "x")) dp,
   "f(&:x)"
  );

  ("assign result of while",
   mk (Seq [
         mk(Assign(fvar 1, `ID_Nil));
         mk(While(id "@y", 
                  mk(Seq [mk(Assign(fvar 1,num 5));
                          mk(Break None)])));
         mk(Assign(id "z", fvar 1))
       ]),
   "z = while @y do break 5 end"
  );

  ("assign result of for",
   mk (Seq [
         mk (Assign(fvar 1, `Lit_Range(false,num 1, num 2)));
         mk (For([`Formal_block_id(`Var_Local, "x")],
                 (fvar 1), (* share the guard through the tmep var*)
                 mk (Seq [
                       (* break translates to explicit assignment *)
                       mk(Assign(fvar 1, num 5));
                       mk(Break(None));
                     ])));
         mk (Assign(id "z", fvar 1))
       ]) ,
   "z = for x in 1..2 do break 5 end"
  );

  ("*uop",
   mc_n "f" [`Star (`ID_Var(`Var_Instance, "@x"))],
   "f *@x"
  );

  ("yield *uop",
   mk (Yield(None, [`Star (id "@x")])),
   "yield *@x"
  );

  ("yield * binop",
   mk (Seq [
         mk (Yield(Some (fvar 2), []));
         mc (Some (fvar 1)) (Some (fvar 2)) (`ID_Operator Op_Times) [id "@x"];
         mk (Expression (fvar 1))
       ]),
   "yield() * @x"
  );

  ("single assign method order",
   mk (Seq [
         mc_s (fvar 1) "w" [];
         mc_s (fvar 3) "y" [];
         mc_s (fvar 2) ~targ:(fvar 3) "h" [];
         mc None (Some (fvar 1)) (`ID_Assign "f") [fvar 2];
       ]),
   "w.f = y.h"
  );
  
  ("multi assign method order",
   mk (Seq [
         mc_s (fvar 2) "y" [];
         mc_s (fvar 1) ~targ:(fvar 2) "h" [];
         mk (Assign(`Tuple [fvar 4; id "z"], fvar 1));
         mc_s (fvar 3)  "w" [];
         mc None (Some (fvar 3)) (`ID_Assign "f") [fvar 4];
       ]),
   "w.f,z = y.h"
  );

  ("multi assign method order 2",
   mk (Seq [
         mc_s (fvar 2) "y" [];
         mc_s (fvar 1) ~targ:(fvar 2) "h" [];
         mc_s (fvar 4) "z" [];
         mc_s (fvar 3) ~targ:(fvar 4) "i" [];
         mk (Assign(`Tuple [fvar 6; fvar 8], `Tuple [fvar 1; fvar 3]));
         mc_s (fvar 5) "w" [];
         mc None (Some (fvar 5)) (`ID_Assign "f") [fvar 6];
         mc_s (fvar 7) "x" [];
         mc None (Some (fvar 7)) (`ID_Assign "g") [fvar 8];
       ]),
   "w.f, x.g = y.h, z.i"
  );

  ("multi assign aset order",
   mk (Seq [
         mc_s (fvar 2) "y" [];
         mc_s (fvar 1) ~targ:(fvar 2) "h" [];
         mc_s (fvar 4) "z" [];
         mc_s (fvar 3) ~targ:(fvar 4) "i" [];
         mk (Assign(`Tuple [fvar 7; fvar 11], `Tuple [fvar 1; fvar 3]));
         mc_s (fvar 5) "w" [];
         mc_s (fvar 6) "a" [num 1];
         mc None (Some (fvar 5)) (`ID_Operator Op_ASet) [fvar 6; fvar 7];
         mc_s (fvar 8) "x" [];
         mc_s (fvar 9) "a" [num 2];
         mc_s (fvar 10) "a" [num 3];
         mc None (Some (fvar 8)) (`ID_Operator Op_ASet) [fvar 9; fvar 10; fvar 11];
       ]),
   "w[a(1)], x[a(2),a(3)] = y.h, z.i"
  );
]

let suite = "Refactor suite" >:::
  List.map refactor_test tests
