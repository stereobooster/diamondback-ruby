open Ast
open Test_helper
open Parse_helper
open OUnit

let test_dir = "parser/large_examples/"

let files = 
  List.sort String.compare (* for a deterministic ordering *)
    (List.filter (fun x -> Filename.check_suffix x ".rb")
	(List.map ((^) test_dir) 
	    (Array.to_list
		(Sys.readdir test_dir)
	    )
	)
    )

let test_file file = 
  let desc = Printf.sprintf "parse file %s" file in
    desc >:: (fun () -> 
		let s = Unix.gettimeofday () in
		  Printf.eprintf "parsing %s...%!" file;
		  ignore(parse_file file);
		  Printf.eprintf " %f\n%!" ((Unix.gettimeofday()) -. s)
	     )

let suite = "Large Tests" >:::
 List.map test_file files

