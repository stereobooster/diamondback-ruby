
open OUnit

let suite = "CFG Suite" >:::
  [
    Printer_tests.suite;
    Refactor_tests.suite;
    Scope_cfg_tests.suite;
    Cfg_locals_test.suite;
  ]
