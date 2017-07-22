
open OUnit

let prog = "../src/early"
let output = "output.rb"

let run_assert cmd = 
  assert_equal 
    ~printer:string_of_int
    ~msg:("subcommand " ^ cmd ^ " failed")
    0 (Sys.command cmd)

let tests () = 
  let () = if Sys.file_exists output then Sys.remove output in
  let files = 
    List.sort String.compare (* for a deterministic ordering *)
      (List.filter
	 (fun x -> 
	    (Filename.check_suffix x ".rb") &&
	      (String.length x > 7) &&
	      (String.sub x 0 4 = "test")
	 )
	 (Array.to_list (Sys.readdir "early"))
      )
  in
    
    List.map
      (fun file -> 
	 file >:: fun () -> 
	   if Sys.file_exists output then Sys.remove output;
	   run_assert (prog ^ " early/" ^ file);
	   run_assert "ruby early/ruby_runner.rb"
      ) files

let suite = "Early Exn" >::: (*tests ()*) []
