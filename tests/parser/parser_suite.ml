
open OUnit

let unit_suite = 
  "Ruby Parser Unit Test Suite" >::: 
    [Parse_disambig.suite;
     Parse_constants.suite;
     Parse_simple.suite;
     Parse_identifier.suite;
     Parse_objects.suite;
     Parse_binop.suite;
     Parse_unary.suite;
     Parse_string.suite;
     Parse_regexp.suite;
     Parse_control.suite;
     Parse_precedence.suite;
     Parse_misc.suite;
     Parse_position.suite;
     Parse_annotation.suite;
    ]

let component_suite = 
  "Ruby Parser Component Suite" >::: 
    (try ignore(Sys.getenv "DO_LARGE"); [Parse_large.suite;]
      with Not_found -> []
    )



let suite = "Parser Test Suite" >:::
  [unit_suite; component_suite]


