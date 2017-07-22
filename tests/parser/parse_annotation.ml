
open Annotation
open Parse_helper
open OUnit

let just_qvar t = QVar t, None

let tvar t = Type_Var (QVar t)

let id_rel t = TIdent_Relative t

let tid t = Type_Ident (id_rel t)

let add_annot_prefix str = 
  let str = Utils.string_chomp str in
  let buf = Buffer.create 32 in 
    Buffer.add_string buf "##%";
    String.iter
      (function 
         | '\n' -> Buffer.add_string buf "\n##% "
         | c -> Buffer.add_char buf c
      ) str;
    Buffer.add_char buf '\n';
    Buffer.contents buf

module L = TypeAnnotParser
let tok_to_string = function
  | L.K_DEF _ -> "K_DEF"
  | L.K_CLASS _ -> "K_CLASS"
  | L.K_MODULE _ -> "K_MODULE"
  | L.K_CAST _ -> "K_CAST"
  | L.K_OR _ -> "K_OR"
  | L.K_SELF _ -> "K_SELF"
  | L.T_BEGIN_LINE _ -> "T_BEGIN_LINE"
  | L.T_NEWLINE _ -> "T_NEWLINE"
  | L.T_SEMICOLON _ -> "T_SEMICOLON"
  | L.T_COLON _ -> "T_COLON"
  | L.T_DOUBLE_COLON _ -> "T_DOUBLE_COLON"
  | L.T_DOT _ -> "T_DOT"
  | L.T_STAR _ -> "T_STAR"
  | L.T_QUESTION _ -> "T_QUESTION"
  | L.T_CARROT _ -> "T_CARROT"
  | L.T_BANG _ -> "T_BANG"
  | L.T_RARROW _ -> "T_RARROW"
  | L.T_LPAREN _ -> "T_LPAREN"
  | L.T_RPAREN _ -> "T_RPAREN"
  | L.T_LESS _ -> "T_LESS"
  | L.T_GREATER _ -> "T_GREATER"
  | L.T_COMMA _ -> "T_COMMA"
  | L.T_LBRACKET _ -> "T_LBRACKET"
  | L.T_RBRACKET _ -> "T_RBRACKET"
  | L.T_LBRACE _ -> "T_LBRACE"
  | L.T_RBRACE _ -> "T_RBRACE"
  | L.T_SUBTYPE _ -> "T_SUBTYPE"
  | L.T_INST_VAR _ -> "T_INST_VAR"
  | L.T_CONST_ID _ -> "T_CONST_ID"
  | L.T_TYPE_ID _ -> "T_TYPE_ID"
  | L.T_METHOD_NAME _ -> "T_METHOD_NAME"
  | L.T_EOF -> "T_EOF"

exception Done
let lextest lexbuf =
  Printf.eprintf "testing...%b\n" lexbuf.Lexing.lex_eof_reached;
  
  try while (*not lexbuf.Lexing.lex_eof_reached;*) true do
    let t = TypeAnnotLexer.token lexbuf in
      Printf.eprintf "got tok: %s\n" (tok_to_string t);
      if t = TypeAnnotParser.T_EOF then raise Done
    done
  with Done -> ()

let parse_annot str = 
  (*lextest (Lexing.from_string str);*)
  let lexbuf = Lexing.from_string str in
    match TypeAnnotParser.input TypeAnnotLexer.token lexbuf with
      | None -> failwith "parser did not produce any annotation"
      | Some a -> a

let annot_test delim embed (desc,annot,ast) = 
  let d = desc ^ ": "  ^ annot in
    d >:: (fun () -> 
             let str = delim ^ (add_annot_prefix annot) in
	     let ast' = parse_annot str in
               assert_equal ast' (embed ast)
                 ~cmp:equal_annotation
                 ~printer:string_of_annotation                 
	  )

let class_test t = annot_test "%class%" (fun x -> ClassType x) t
let method_test t = annot_test "%def%" (fun x -> MethodType x) t

let expr_test (desc,annot,ast) = 
  let d = desc ^ ": "  ^ annot in
    d >:: (fun () -> 
	     let ast' = parse_annot ("%cast%" ^ annot) in
               assert_equal ast' (ExprType ast)
                 ~cmp:equal_annotation
                 ~printer:string_of_annotation
	  )

let class_annot_tests = [
  ("parametric class",
   "A<t>",
   ("A",[just_qvar "t"],[])
  );

  ("bounded quant class",
   "A<t>; t <= B",
   ("A",[QVar "t", Some (tid "B")],[])
  );

  ("2var bounded quant class",
   "A<t,u>; t <= u",
   ("A",[QVar "t", Some (tvar "u");just_qvar "u"],[])
  );

  ("absolute bounded quant class",
   "A<t>; t <= ::B",
   ("A",[QVar "t", Some (Type_Ident (TIdent_Absolute "B"))],[])
  );

  ("scoped relative bounded quant class",
   "A<t>; t <= B::C",
   ("A",[QVar "t", Some (Type_Ident (TIdent_Scoped(TIdent_Relative "B","C")))],[])
  );

  ("scoped absolute bounded quant class",
   "A<t>; t <= ::B::C",
   ("A",[QVar "t", Some (Type_Ident (TIdent_Scoped(TIdent_Absolute "B","C")))],[])
  );

  ("class with 3 universal params",
   "A<t1, t2, t3'>",
   ("A",[just_qvar "t1";just_qvar "t2";just_qvar "t3'"],[])
  );
  
  ("class with 3 bounded params",
   "A<t1, t2, t3'>; t1 <= U1, t2 <= ::U2, t3' <= U4::U3",
   ("A",[(QVar "t1",Some (tid "U1"));
         (QVar "t2",Some (Type_Ident (TIdent_Absolute "U2")));
         (QVar "t3'",Some (Type_Ident (TIdent_Scoped(TIdent_Relative "U4","U3"))));
        ],[])
  );

  ("class with param type",
   "A<^blah>",
   ("A", [QParam "blah", None],[])
  );

  ("declared subtype",
   "A <= B",
   ("A", [],[tid "B"])
  );

  ("declared multiple subtypes",
   "A <= B, C",
   ("A", [],[tid "B";tid "C"])
  );

  ("parameterized declared subtype",
   "A<t> <= B",
   ("A", [just_qvar "t"],[tid "B"])
  );

  ("parameterized declared param subtype",
   "A<t> <= B<t>",
   ("A", [just_qvar "t"],[Type_App(id_rel "B", [tvar "t"])])
  );

  ("parameterized declared multiple subtypes",
   "A<t> <= B, C",
   ("A", [just_qvar "t"],[tid "B";tid "C"])
  );

  ("delcared subtype of object type",
   "A <= [f: () -> Fixnum]",
     let f = id_rel "f",[], MethodSig([],None,tid "Fixnum") in
       ("A",[],[Type_Object([], [f])])
  );
         
]

let method_annot_tests = [

  ("monomorphic method, no block, no args",
   "f: () -> Fixnum",
   [(id_rel "f",[],MethodSig([],None,tid "Fixnum")) ]
  );

  ("monomorphic method, no block, no parens",
   "f: Fixnum -> Fixnum",
   [(id_rel "f",[],MethodSig([tid "Fixnum"],None,tid "Fixnum")) ]
  );

  ("monomorphic method, no block, parens",
   "f: (Fixnum) -> Fixnum",
   [(id_rel "f",[],MethodSig([tid "Fixnum"],None,tid "Fixnum")) ]
  );

  ("monomorphic method, no block, parens, multiarg",
   "f: (Fixnum,Float) -> Fixnum",
   [(id_rel "f",[],MethodSig([tid "Fixnum";tid "Float"],None,tid "Fixnum")) ]
  );

  ("monomorphic method, block, no parens",
   "f: Fixnum {Fixnum -> Fixnum} -> Fixnum",
   [(id_rel "f",[],MethodSig([tid "Fixnum"],
                             Some (MethodSig([tid "Fixnum"],None,tid "Fixnum")),
                             tid "Fixnum")) ]
  );

  ("monomorphic method, block, parens",
   "f: (Fixnum) {Fixnum -> Fixnum} -> Fixnum",
   [(id_rel "f",[],MethodSig([tid "Fixnum"],
                             Some (MethodSig([tid "Fixnum"],None,tid "Fixnum")),
                             tid "Fixnum")) ]
  );

  ("monomorphic method, block w parens",
   "f: (Fixnum) {(Fixnum,Float) -> Fixnum} -> Fixnum",
   [(id_rel "f",[],MethodSig([tid "Fixnum"],
                             Some (MethodSig([tid "Fixnum";tid "Float"],
                                             None,tid "Fixnum")),
                             tid "Fixnum")) ]
  );

  ("polymorphic method 1 arg",
   "f<t> : t -> t",
   [(id_rel "f",[just_qvar "t"],MethodSig([tvar "t"],None,tvar "t") )]
  );

  ("polymorphic method 2 arg, parens",
   "f<t,u> : (t,u) -> t",
   [(id_rel "f",[just_qvar "t";just_qvar "u"],
     MethodSig([tvar "t";tvar "u"],None,tvar "t") )]
  );

  ("bounded polymorphic method",
   "f<t> ; t <= Fixnum : t -> t",
   [(id_rel "f",[QVar "t", Some (tid "Fixnum")],
     MethodSig([tvar "t"],None,tvar "t") )]
  );

  ("2arg bounded polymorphic method",
   "f<t,u> ; t <= u : t -> t",
   [(id_rel "f",[(QVar "t", Some (tvar "u")); just_qvar "u"],
     MethodSig([tvar "t"],None,tvar "t") )]
  );

  ("intersection method",
   "f: Fixnum -> Fixnum\nf: Float -> Float",
   [(id_rel "f",[], MethodSig([tid "Fixnum"],None,tid "Fixnum"));
    (id_rel "f",[], MethodSig([tid "Float"],None,tid "Float"))
   ]
  );

  ("intersection method with quants",
   "f<t>: Fixnum -> Fixnum\nf<t>: Float -> Float",
   [(id_rel "f",[just_qvar "t"], MethodSig([tid "Fixnum"],None,tid "Fixnum"));
    (id_rel "f",[just_qvar "t"], MethodSig([tid "Float"],None,tid "Float"))
   ]
  );

  ("dynamic arg and ret",
   "f: ? -> ?",
   [(id_rel "f",[], MethodSig([Type_Dynamic],None,Type_Dynamic))]
  );

  ("optional arg",
   "f: ?Fixnum -> ?",
   [(id_rel "f",[], MethodSig([Type_Optional (tid "Fixnum")],None,Type_Dynamic))]
  );

  ("varargs arg",
   "f: *Fixnum -> ?",
   [(id_rel "f",[], MethodSig([Type_Varargs (tid "Fixnum")],None,Type_Dynamic))]
  );

  ("all kinds of args",
   "f: (A,B,?C,*D) -> ?",
   [(id_rel "f",[], MethodSig([tid "A";tid "B";Type_Optional (tid "C");
                               Type_Varargs (tid "D")],None,Type_Dynamic))]
  );

  ("object type just method",
   "f: () -> [g: Fixnum -> Fixnum]",
   [
     let g = id_rel "g",[], MethodSig([tid "Fixnum"],None,tid "Fixnum") in
       (id_rel "f",[], MethodSig([],None,Type_Object([], [g])))]
  );

  ("object type just field",
   "f: () -> [@g: Fixnum]",
   [(id_rel "f",[], 
     MethodSig([],None,Type_Object(["@g",tid "Fixnum"],[])))]
  );

  ("object type, field meth field",
   "f: () -> [@g: Fixnum, h: Fixnum -> Fixnum, @i: Fixnum]",
   [let g = "@g",tid "Fixnum" in
    let h = id_rel "h",[], MethodSig([tid "Fixnum"],None,tid "Fixnum") in
    let i = "@i",tid "Fixnum" in
      (id_rel "f",[], MethodSig([],None,Type_Object([g;i],[h])))]
  );

  ("object type, meth field meth",
   "f: () -> [a:Fixnum -> Fixnum, @b: Fixnum, c:Fixnum -> Fixnum]",
   [let a = id_rel "a",[], MethodSig([tid "Fixnum"],None,tid "Fixnum") in
    let b = "@b",tid "Fixnum" in
    let c = id_rel "c",[], MethodSig([tid "Fixnum"],None,tid "Fixnum") in
      (id_rel "f",[], MethodSig([],None,Type_Object([b],[a;c])))]
  );

  ("capital method name",
   "F : () -> Fixnum",
   [id_rel "F",[], MethodSig([],None,tid "Fixnum")]
  );

  ("scoped = method name",
   "A.foo= : () -> Fixnum",
   [TIdent_Scoped(id_rel "A","foo="),[], MethodSig([],None,tid "Fixnum")]
  );

  ("scoped aref",
   "A.\"[]\" : () -> Fixnum",
   [TIdent_Scoped(id_rel "A", "[]"), [], MethodSig([],None,tid "Fixnum")]
  );

  ("bounded object type",
   "max<v,u> ; u <= [z : Fixnum -> Fixnum ] : () -> u ",
   let meth = id_rel "z",[], MethodSig([tid "Fixnum"],None,tid "Fixnum") in
     [id_rel "max", [just_qvar "v";(QVar "u",Some (Type_Object([],[meth])))],
      MethodSig([],None,tvar "u")]
  );

  ("return upper bound",
   "f<u,self> ; self <= u : () -> u",
   [id_rel "f",[just_qvar "u";(QSelf,Some (tvar "u"))],MethodSig([],None,tvar "u")]
  );

  ("single union arg",
   "f : Fixnum or Float -> Fixnum",
   [id_rel "f", [], 
    MethodSig([Type_Union([tid "Fixnum";tid "Float"])], None,tid "Fixnum")]
  );

  ("fixme",
   "f : !FIXME -> !FIXME",
   [id_rel "f", [], MethodSig([Type_Fixme],None, Type_Fixme)]
  );

  ("app with type var",
   "f<t> : A<t> -> A<t>",
   [let at = Type_App(id_rel "A",[tvar "t"]) in
      (id_rel "f", [just_qvar "t"], MethodSig([at],None,at))]
  );

  ("app with class",
   "f : () -> A<Fixnum>",
   [let at = Type_App(id_rel "A",[tid "Fixnum"]) in
      (id_rel "f", [], MethodSig([],None,at))]
  );

  ("method with param type",
   "f<^args,ret> : (^args) -> ret",
   [id_rel "f", [(QParam "args",None);just_qvar "ret"],
    MethodSig([Type_Var (QParam "args")],None, tvar "ret")]
  );

  ("param list",
   "f: A<^(Fixnum,Fixnum)> -> Fixnum",
   [id_rel "f", [], 
    MethodSig([Type_App(id_rel "A", [Type_ParamList [tid "Fixnum";tid "Fixnum"]])],
              None, tid "Fixnum")]
  );

  ("tuple",
   "f : () -> (Fixnum,String)",
   [id_rel "f", [], MethodSig([],None, Type_Tuple [tid "Fixnum"; tid "String"])]
  );
]

let expr_annot_tests = [
  ("cast identifier",
   "String",
   tid "String"
  );
]

let ignores = [
]


let suite = "Annotation Tests" >:::
  ["Class Annotations" >::: List.map class_test class_annot_tests;
   "Method Annotations" >::: List.map method_test method_annot_tests;
   "Expr Annotations" >::: List.map expr_test expr_annot_tests;
  ]
