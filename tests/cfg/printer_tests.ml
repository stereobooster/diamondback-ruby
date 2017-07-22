
open OUnit
open Cfg_test_helper

let chomp s = 
  let len = String.length s in
    if len < 1 then s
    else if s.[len-1] == '\n' 
    then String.sub s 0 (len-1)
    else s
    

let print_test (desc,result,code) = 
  let d = desc ^ ": "  ^ code in
    d >:: (fun () -> 
             Cfg_refactor.re_init ();
             let cfg = refactor_string code in
             let code_str = Cfg_printer.CodePrinter.string_of_cfg cfg in
             let code_str = chomp code_str in
	       assert_equal ~cmp:(=) ~printer:(fun x -> "'" ^ x ^ "'")
                 result code_str
	  )



let tests = [
  ("don't escape {}s in outer ctx",
   "%r{\\d{4,}}",
   "/\\d{4,}/"
  );

  ("escape {} inside []s",
   "%r{[^\\}]}",
   "/[^}]/"
  );
  
  ("leave double slash",
   "%r{[^\\\\]}",
   "/[^\\\\]/"
  );

  ("preserve float precision",
   "3.14159265359",
   "3.14159265359"
  );
]

let suite = "Printer suite" >:::
  List.map print_test tests
