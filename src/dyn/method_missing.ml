open Config
open Printf
open Dynamic
open Yaml
open Utils
open Dyn_builder

module Domain = YString
module CoDomain = YString

let name = "method_missing"

let instrument_ast e = e

open Cfg
module C = Cfg.Abbr

(* DRuby::Method_Missing.watch(file,loc,self,name) *)
let watch pos name = 
  let targ = C.access_path ["DRuby";"Profile";"Method_missing"] in
  let args = [C.str (File_loader.normalize_filename pos.Lexing.pos_fname); 
              C.num pos.Lexing.pos_lnum;
              `ID_Self; 
              C.str "method_missing"; 
              C.local name] 
  in
    C.mcall ~targ (`ID_MethodName "watch") args pos 

class mm_visitor = 
object(self)
  inherit default_visitor
  method visit_stmt s = match s.snode with
      (* insert a 'watch' call at the beginning of every definition
         of a method_missing method *)
    | Method((Instance_Method(`ID_MethodName "method_missing") as mm),
             ((`Formal_meth_id mname)::_ as args),body) ->
        let watch_call = watch s.pos mname in
        let body' = visit_stmt (self:>cfg_visitor) body in
        let body' = C.seq [watch_call;body'] s.pos in
        let meth' = mkstmt (Method(mm,args,body')) s.pos in
          if conf.mine_dyn_feats then (* mining *)
            Mining.mine
              s.pos.Lexing.pos_fname 
              s.pos.Lexing.pos_lnum 
              mname
          else ();
          Visitor.ChangeTo meth'

    | Method(Singleton_Method(_,`ID_MethodName "method_missing"),_,_) ->
        Log.fatal (Log.of_loc s.pos) "singleton method_missing"
          
    | _ -> Visitor.DoChildren

end

let instrument_cfg s = visit_stmt (new mm_visitor) s

let transform_ast t e = e 

class mm_inserter yaml_map = 
object(self)
  inherit default_visitor
  method visit_stmt s = match s.snode with
    | Method(Instance_Method(`ID_MethodName "method_missing"),
             ((`Formal_meth_id mname)::rest_args),body) -> begin
        try
          let body = dead_method "method_missing" [C.str mname] s.pos in
          let args = (`Formal_meth_id mname)::rest_args in
          let new_mm = C.mdef "method_missing" args body s.pos in
          let meths = yaml_map "method_missing" s.pos in
          let meth_list = List.fold_left
            (fun acc missed -> 
               let preset = C.assign (C.local mname) (C.atom missed) s.pos in
               let body' = C.seq [preset;body] s.pos in
               let meth' = C.mdef missed rest_args body' s.pos in
                 meth'::acc
            ) [new_mm] meths
          in
            Visitor.ChangeTo (C.seq meth_list s.pos)
        with Not_found -> Visitor.DoChildren
      end
        
    | _ -> Visitor.DoChildren
end

let transform_cfg t s = 
  visit_stmt (new mm_inserter t) s 
