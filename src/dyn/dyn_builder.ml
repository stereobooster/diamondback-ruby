
open Utils
open Cfg_printer
open Config


type watch_redir = {
  src_msg : Cfg.msg_id;
  dst_target : Cfg.expr;
  dst_msg : Cfg.msg_id;
}

let msg_str m = format_to_string CodePrinter.format_msg_id m

open Cfg

(** A utility class for capturing arguments sent to specific methods
    at runtime.  For any method call e.src_msg, a new call is inserted
    before that call to

    dst_target.dst_msg(file,line,e,*args)

    where *args are the arguments passed to e.m
*)
class watch_visitor ?(rec_lit_str_and_atom=true) insts = 
  let map = List.fold_left
    (fun acc t ->
       let msg = msg_str t.src_msg in
         try StrMap.add msg (t::StrMap.find msg acc) acc
         with Not_found -> StrMap.add msg [t] acc
    ) StrMap.empty insts 
  in
  let check_mc mc = 
    if rec_lit_str_and_atom then ()
    else
      match mc with
        | {mc_args=(`Lit_String _ | `Lit_Atom _)::_} ->
            raise Not_found
        | _ -> ()
  in
object(self)
  inherit default_visitor
    
  method visit_stmt s = match s.snode with
    | MethodCall(lhs,mc) -> begin try
        let () = check_mc mc in
        let fname = File_loader.normalize_filename s.pos.Lexing.pos_fname in
        let lnum = s.pos.Lexing.pos_lnum in
        let method_name = msg_str mc.mc_msg in
          (* exn jumps below and skips this MC *)
        let lst = StrMap.find method_name map in
        let targ = default_opt `ID_Self mc.mc_target in
        let extra = List.fold_left
          (fun acc t -> 
             let args = 
               (`Lit_String fname)
               ::(`Lit_FixNum lnum)
               ::(targ :> star_expr)
               ::(`Lit_Atom method_name)
               ::mc.mc_args
             in
             let mc = {mc_target = Some t.dst_target;
                       mc_msg = t.dst_msg;
                       mc_args = args;
                       mc_cb = mc.mc_cb}
             in
               (mkstmt (MethodCall(None,mc)) s.pos)::acc
          ) [s] lst
        in match extra with
          | [] -> Visitor.DoChildren
          | lst ->  
              if conf.mine_dyn_feats then (* mining *)
                  Mining.mine fname lnum method_name
              else ();
              Visitor.ChangeTo (mkstmt (Seq extra) s.pos)
      with Not_found -> Visitor.DoChildren
      end

    | _ -> Visitor.DoChildren
        
end

let dead_method m args pos = 
  let mc = {
    mc_target=Some (Cfg.Abbr.access_path ["DRuby";"Profile";"Runtime"]);
    mc_msg = `ID_MethodName "dead_method";
    mc_args = (`Lit_String m)::args;
    mc_cb = None;
  }
  in mkstmt (MethodCall(None,mc)) pos

module type CaseSig = sig  
  module Domain : Yaml.YType
  module Codomain : Yaml.YType

  val preamble : Cfg.stmt list
  val case_guard : Cfg.expr
  val when_guard : Codomain.t -> Cfg.expr
  val when_body : Codomain.t -> Cfg.stmt
  val else_body : Cfg.stmt
end

module C = Cfg.Abbr

module CaseTransformer(S : CaseSig) : sig
  
  val transform: (S.Domain.t -> Cfg.pos -> S.Codomain.t list) -> 
    S.Domain.t -> Cfg.pos -> stmt Visitor.visitAction

end = struct
  let transform yaml_map key pos = 
    let guard = S.case_guard in
    let whens = 
      List.map
        (fun v -> 
           S.when_guard v, S.when_body v
        ) (yaml_map key pos) 
    in
    let default = S.else_body in
    let case = C.case guard whens ~default pos in
      match S.preamble with
        | [] -> Visitor.ChangeTo case
        | lst -> Visitor.ChangeTo (C.seq (lst@[case]) pos)
end
