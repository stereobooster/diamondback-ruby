
open Parse_helper
open Printf
open Cfg
open Cfg_printer

let _ = 
  if (Array.length Sys.argv) != 2 
  then begin
    eprintf "Usage: print_cfg <ruby_file> \n";
    exit 1
  end;
  let fname = Sys.argv.(1) in
  let ast = 
    try Parse_helper.parse_file fname
    with Invalid_argument(_) -> parse_file fname
  in
(*  let loader = File_loader.create File_loader.EmptyCfg "../std-lib" in*)
  let stmt = Cfg_refactor.refactor_ast ast in
(*  let stmt = Cfg_scope.resolve_scopes loader stmt in*)
  let oc = open_out "output.rb" in
  let () = CodePrinter.print_stmt oc stmt in
  let () = CodePrinter.print_stmt stdout stmt in
  let () = close_out oc in
    ()
      (*
  let () = compute_cfg stmt in

  let _,_ = Dataflow.DataTypeFlowDF.fixpoint stmt in
  let _, out_tbl = Dataflow.EarlyCastDF.fixpoint stmt in
    Hashtbl.iter
      (fun stmt t ->
	let s1 = Cfg_printer.string_of_cfg stmt in
	let s2 = Dataflow.EarlyCast.to_string t	in
	  printf "[%s]: %s\n\n" s1 s2
		  
      ) out_tbl
      *)
