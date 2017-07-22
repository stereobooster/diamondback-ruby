
open Ast
open Test_helper
open Parse_helper
open OUnit
open Printf


let tests = [
  ("Single quote", [sing_str "test"], "'test'");
  ("Double quote", [doub_str "test"], "\"test\"");
  ("Back tick", [tick_str "test"], "`test`");
  ("Heredoc", [doub_str "test\n"], "<<DELIM
test
DELIM");
  ("Heredoc", [E_Return([doub_str "test\n"], dp)],
   "return <<__FOO__\ntest\n__FOO__");
  ("Heredoc", [doub_str "test\n"], "<<-\"end;\"
test
end;");

  ("Heredoc single-str delim, no interp",
   [sing_str "#{z}\n"],
   "<<'EOF'\n#{z}\nEOF"
  );

  ("consecutive single strings",[sing_str "test"],"'te' 'st'");
  ("consecutive double strings",
   [ast_str (String_Double [StrChars "te";StrChars "st"])],
    "\"te\" \"st\"");

  ("Single quote %q", [sing_str "test"], "%q(test)");
  ("Double quote %Q", [doub_str "test"], "%Q(test)");
  ("User quote %!", [doub_str "test"], "%!test!");

  ("interp dstring",
  [ast_str (String_Double [StrChars "hi ";StrExpr (ast_id "x")])],
  "\"hi #{x}\"");

  ("espaced non-interp dstring",
  [ast_str (String_Double [StrChars "hi \\#{x}"])],
  "\"hi \\#{x}\"");

  ("brace in interp user string",
   [E_Literal(Lit_String (String_Double [StrExpr (doub_str "}")]),dp)],
   "%{#{\"}\"}}"
  );

  ("array string %w", 
   [E_Array([sing_str "a"; sing_str "b"; sing_str "c"],dp)], 
  "%w(a b c)");

  ("empty array string %w", 
   [E_Array([],dp)], 
  "%w\"\"");

  ("array string %W",
   [E_Array([doub_str "a"; doub_str "b"; doub_str "c"],dp)], 
  "%W(a b c)");

  ("array string %W with #{}", 
  [E_Array([doub_str "a";
	    ast_str (String_Double [StrChars "b";StrExpr (ast_id "c")])]
	     ,dp)], 
  "%W(a b#{c})");

  ("Back tick %x", [tick_str "test"], "%x{test}");

  ("embedded {}", 
  [ast_str (String_Double [StrChars "hi ";
                           StrExpr (E_Hash(true,[ast_id "x";ast_id"y"],dp))
                          ])],
  "%{hi #{{x,y}}}");

  ("double string with escaped nl",
   [doub_str "hi there"],
   "\"hi\\\n there\""
  );

  ("comment after heredoc",
   [E_Binop(ast_id "x",Op_ASSIGN,doub_str "3\n",dp)],
   "x = <<EOF #hi\n3\nEOF"
  );

  ("stacked heredocs",
   [E_MethodCall(ast_id "f",[doub_str "a\n";doub_str "b\n"],None,dp);
    ast_id "g"],
   "f <<A, <<B\na\nA\nb\nB\n g"
  );

  ("double nested inside of heredoc",
   [E_Literal
      (Lit_String
         (String_Double [StrExpr (doub_str "\n");StrChars "\n"]),dp)],
   "<<EOM\n#{\"\n\"}\nEOM\n"
  );

  ("heredoc interp",
   [ast_str 
      (String_Double [StrExpr 
                        (E_MethodCall(ast_id "f", [], 
                                      Some (E_CodeBlock(true,None,[ast_num 3],dp)),
                                      dp));
                      StrChars "\n"])],
   "<<E\n#{f {3}}\nE"
  );
  
  ("heredoc, then multi-line regexp",
   [E_Binop(E_Tuple([ast_id "a";ast_id "b"],dp),
           Op_ASSIGN,
           E_Tuple([doub_str "hi\n";ast_re "\n 4\n" "x"],dp),
           dp)],
   "a,b = <<EOF, /\nhi\nEOF\n 4\n/x"
  );

  ("heredoc, then multi-line regexp, escape newline",
   [E_Binop(E_Tuple([ast_id "a";ast_id "b"],dp),
           Op_ASSIGN,
           E_Tuple([doub_str "hi\n";ast_re " 4\n" "x"],dp),
           dp)],
   "a,b = <<EOF, /\\\nhi\nEOF\n 4\n/x"
  );

  ("heredoc then \"-string with newline",
   [E_Binop(E_Tuple([ast_id "a";ast_id "b"],dp),
           Op_ASSIGN,
           E_Tuple([doub_str "first\n";doub_str "\nsecond"],dp),
           dp)],
   "a,b = <<EOF, \"\nfirst\nEOF\nsecond\""
  );

  ("heredoc then \"-string with escaped newline",
   [E_Binop(E_Tuple([ast_id "a";ast_id "b"],dp),
           Op_ASSIGN,
           E_Tuple([doub_str "first\n";doub_str "second"],dp),
           dp)],
   "a,b = <<EOF, \"\\\nfirst\nEOF\nsecond\""
  );

  ("heredoc then '-string with newline",
   [E_Binop(E_Tuple([ast_id "a";ast_id "b"],dp),
           Op_ASSIGN,
           E_Tuple([doub_str "first\n";sing_str "\nsecond"],dp),
           dp)],
   "a,b = <<EOF, '\nfirst\nEOF\nsecond'"
  );

  ("heredoc then '-string with escaped newline",
   [E_Binop(E_Tuple([ast_id "a";ast_id "b"],dp),
           Op_ASSIGN,
           E_Tuple([doub_str "first\n";sing_str "\\\nsecond"],dp),
           dp)],
   "a,b = <<EOF, '\\\nfirst\nEOF\nsecond'"
  );

  ("heredoc then %{}-string with newline",
   [E_Binop(E_Tuple([ast_id "a";ast_id "b"],dp),
           Op_ASSIGN,
           E_Tuple([doub_str "first\n";doub_str "\nsecond"],dp),
           dp)],
   "a,b = <<EOF, %{\nfirst\nEOF\nsecond}"
  );

  ("heredoc then %{}-string with escaped newline",
   [E_Binop(E_Tuple([ast_id "a";ast_id "b"],dp),
           Op_ASSIGN,
           E_Tuple([doub_str "first\n";doub_str "second"],dp),
           dp)],
   "a,b = <<EOF, %{\\\nfirst\nEOF\nsecond}"
  );

  ("heredoc then regexp with newline",
   [E_Binop(E_Tuple([ast_id "a";ast_id "b"],dp),
           Op_ASSIGN,
           E_Tuple([doub_str "first\n";ast_re "\nsecond" ""],dp),
           dp)],
   "a,b = <<EOF, /\nfirst\nEOF\nsecond/"
  );

  ("heredoc then regexp with escaped newline",
   [E_Binop(E_Tuple([ast_id "a";ast_id "b"],dp),
           Op_ASSIGN,
           E_Tuple([doub_str "first\n";ast_re "second" ""],dp),
           dp)],
   "a,b = <<EOF, /\\\nfirst\nEOF\nsecond/"
  );

]

let test_parse_string_user_matched () = 
  let ast = [doub_str "test"] in
  let d_ast d1 d2 = parse_string (sprintf "%%%ctest%c" d1 d2) in
    "User String Matched Delim" >:::
      List.map 
      (fun (d1,d2) ->
	let func = fun () ->
	  let ast' = d_ast d1 d2 in
	    assert_ast_equal ast ast'
	in (sprintf "delim %c %c" d1 d2) >:: func
      ) [ '{','}'; '<','>'; '(',')'; '[',']'; ]

let test_parse_string_user_single () = 
  let ast = [doub_str "test"] in
  let d_ast d = parse_string (sprintf "%%%ctest%c" d d) in
  let string_single_delim = [
      '!'; '@'; '#'; '$'; '%'; '^'; '&'; '*';
      ','; '.'; '?'; '`'; '~'; '|'; '+'; '_';
      '-'; '\\'; '/'; ':'; '"'; '\'';]
  in
    "User String Single Delim" >:::
      List.map 
      (fun delim ->
	let func = fun () ->
	  let ast' = d_ast delim in
	    assert_ast_equal ast ast'
	in (sprintf "delim %c" delim) >:: func
      ) string_single_delim

let suite = "String Literal Parse Tests" >:::
  test_parse_string_user_single() ::
  test_parse_string_user_matched() ::
  List.map simple_test tests


