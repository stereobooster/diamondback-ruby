

module type ProgSuite = sig

  val setup : string -> unit
  val teardown : string -> unit

  val dir : string
  val success_dir : string
  val fail_dir : string

end

module Make(PS : ProgSuite) = struct
  
  open Parse_helper
  open OUnit
  open Config

  let ls dir = 
    List.sort String.compare (* for a deterministic ordering *)
      (List.filter (fun x -> Filename.check_suffix x ".rb")
	 (List.map (Filename.concat dir) 
	    (Array.to_list
	       (Sys.readdir dir)
	    )
	 )
      )

  let base_cfg () : Cfg.stmt * string = 
    let ic = open_in "base_types.cfg" in
    let cfg = Marshal.from_channel ic in
      cfg, "base_types.rb"

  let nano_cfg () : Cfg.stmt * string = 
    let file = "typing/nanotypes.rb" in
    let ast = parse_file file in
    let cfg = Cfg_refactor.refactor_ast ast in
      cfg, file

  let build_cfg loader file = 
    let module DList = 
      Dynamic.Cons(Method_missing)
        (Dynamic.Cons(Evals)
           (Dynamic.Cons(Send)
              (Dynamic.Singleton(Reflection))))
    in
    let module Dyn = Dynamic.Make(DList) in
    let cfg = 
      if conf.profile
      then Dyn.run loader file
      else File_loader.load_file loader file
    in
      cfg

  let process_file fname = 
    PS.setup fname;
    let result = 
    try
      conf.error_raises_exc <- true;
      let loader = File_loader.create Typing.c_handler ["../lib"] in
      let cfg0, name = 
        try ignore(Sys.getenv "DO_LARGE"); base_cfg ()
        with Not_found -> nano_cfg ()
      in
      let env = Typing.type_cfg loader name cfg0 in
      let cfg1 = build_cfg loader fname in
      let env = Typing.type_in_env env fname cfg1 in
        Typing.solve_constraints env;
        None
    with e -> Some e
    in
      PS.teardown fname;
      match result with None -> () | Some e -> raise e

  exception Failed

  let test_fail file = 
    let desc = Printf.sprintf "expect fail file %s" file in
      desc >:: 
        (fun () -> 
	   try ignore(process_file file); raise Failed
	   with 
	     | Failed -> failwith "succeeded, but expected failure"
	     | _ -> ()
        )

  let test_succeed file = 
    let desc = Printf.sprintf "expect succeed file %s" file in
      desc >:: (fun () -> ignore(process_file file))

  let tests_list = 
    [List.map test_succeed (ls PS.success_dir);
     List.map test_fail (ls PS.fail_dir);
    ]

  let suite = ("Type " ^ PS.dir ^ " Progs") >:::
    List.fold_left List.rev_append [] tests_list
    
end
