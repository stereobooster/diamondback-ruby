
open OUnit

let suite = "Whole project test suite" >:::
  [Typing_suite.suite;
   Cfg_suite.suite;
   Parser_suite.suite;
   Profile_suite.suite;
   Yaml_suite.suite;
   Contracts_suite.suite;
  ]

let _ = 
  if OUnit_success.was_successful (run_test_tt suite)
  then ()
  else exit 1
