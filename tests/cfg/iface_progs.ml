open OUnit
open Config

module IfaceProgs = struct
  let dir = "cfg"
  let success_dir = "cfg/iface_succeed"
  let fail_dir = "cfg/iface_fail"
  let setup _ = ()
  let teardown _ = ()

  (* Tests are directories containing multiple source files.
   * The first file processed is dir_name/dir_name.rb *)
  let find_files top_dir =
    let dirs = Array.to_list (Sys.readdir top_dir) in
    let dirs = List.filter (fun x -> Filename.basename x <> ".svn") dirs in
    let get_rb dir_name =
      Filename.concat
        (Filename.concat top_dir dir_name)
        (dir_name ^ ".rb")
    in
      List.sort String.compare (List.map get_rb dirs)

end

module IfaceTests = Prog_test.Make(IfaceProgs)

let suite = "Type Progs" >:::
  [IfaceTests.suite]
