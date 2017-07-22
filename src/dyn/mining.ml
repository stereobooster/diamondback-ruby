(** This file contains functions that deal with mining of SYNTACTIC OCCURENCES
    of dynamic features by DRuby. Also, it stores the path that contains all
    the yaml files profiled by DRuby which will be used by running script to
    make a use of the gathered files.  *)

open Format
open Utils
open Config
open Yaml

(* kind, fname, lineno, meth_name *)
module SyntOcc = YList(Y4Tuple(YString)(YString)(YInt)(YString))

(** syntactic occurrences of dynamic language constructs *)
let syn_occs = ref ([] : SyntOcc.t)

(** updates the syntactic occurrences list *)
let mine fname lineno mname =
  let kind =
    if List.mem mname ["instance_eval"; "class_eval"; "module_eval"; "eval"] 
    then "eval"
    else if List.mem mname ["send"; "__send__"] 
    then "send"
    else if List.mem mname ["method_missing"] 
    then "method_missing"
    else if List.mem mname  ["class_variable_get"; "class_variable_set";
                             "instance_variable_get"; "instance_variable_set";
                             "const_get"; "const_set"; 
                             "attr"; "attr_accessor"; 
                             "attr_reader"; "attr_writer"; ] 
    then "reflection"
    else if List.mem mname ["require"; "load"] 
    then "require"
    else "method_missing"
  in
  let fname = File_loader.normalize_filename fname in
    syn_occs := (kind, fname, lineno, mname)::!syn_occs

let reset () = syn_occs := ([] : SyntOcc.t) 

(** outputs the result into a yaml file  *)
let output_to_yaml fname = 
  (* let oc = open_out_gen [Open_creat;(* Open_append *)] 0o666 fname in *)
  let oc = open_out fname in
    Yaml.emit_yaml_channel oc (SyntOcc.to_yaml !syn_occs);
    close_out oc
