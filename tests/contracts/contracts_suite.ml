
open OUnit
open Config
open Printf

module ContractProgs = struct
  let dir = "contracts"
  let success_dir = "contracts/succeed"
  let fail_dir = "contracts/fail"
  let old_emit = ref false
  let setup _ = 
    old_emit := conf.emit_runtime_casts;
    conf.emit_runtime_casts <- true

  let all_lines ic = 
    let buf = Buffer.create 1020 in
    let str_size = 32 in
    let str = String.create str_size in
    let rec read_lines () = 
      let read = input ic str 0 str_size in
        if read = 0 then Buffer.contents buf
        else begin
          Buffer.add_substring buf str 0 read;
          read_lines ()
        end
    in read_lines ()

  let run_contract file expected = 
    let ic = Unix.open_process_in ("ruby -I../lib -rcasts " ^ file ^ " 2>&1") in
    let output = all_lines ic in
      match Unix.close_process_in ic with
        | Unix.WEXITED 0 -> ()
        | _ -> failwith (file ^ " failed to run:\n" ^ output)
        
  let matches re str = 
    try ignore(Str.search_forward re str 0); true
    with Not_found -> false

  let succ_re = Str.regexp "/succeed/"
  let fail_re = Str.regexp "/fail/"
      

  let expectation file = 
    if matches succ_re file then true
    else if matches fail_re file then false
    else assert false
    
  let teardown file = 
    conf.emit_runtime_casts <- !old_emit;
    run_contract file (expectation file)

end

module ContractTests = Prog_test.Make(ContractProgs)

let suite = "Contracts Suite" >:::
  [ContractTests.suite;
  ]
