

open OUnit

let suite = 
  "Typing" >::: 
    [
      Type_progs.suite;
      (*Access_tests.suite;*)
    ]



