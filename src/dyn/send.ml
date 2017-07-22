open Config
open Yaml
open Dyn_builder

module Domain = YString
module CoDomain = YString

let name = "send"

open Ast

module C = Cfg.Abbr
let instrument_ast a = a

let watch_f meth = 
  {src_msg = `ID_MethodName meth;
   dst_target = C.access_path ["DRuby";"Profile";"Send"];
   dst_msg = `ID_MethodName "watch";
  }

let instrument_cfg cfg = 
  let watched = [watch_f "send"; watch_f "__send__"] in
    Cfg.visit_stmt (new watch_visitor watched) cfg

module Base = struct
  module Domain = Domain
  module Codomain = CoDomain
  let preamble = []
  let when_guard str = C.str str
end

open Cfg
class send_visitor yaml_map = 
object(self)
  inherit Cfg.default_visitor as super
  method visit_stmt s = match s.snode with
    | MethodCall(lhs,mc) -> begin match mc with
        | {mc_msg=`ID_MethodName ("send"|"__send__" as send);
           mc_args=(#expr as arg)::rest} ->
            let module M = struct
              include Base
              let case_guard = arg
              let else_body = dead_method send [(arg :> star_expr)] s.pos
              let when_body str = 
                C.mcall ?lhs ?targ:mc.mc_target (msg_id_of_string str) rest s.pos
            end in let module T = CaseTransformer(M) in
              T.transform yaml_map send s.pos
        | _ -> super#visit_stmt s
      end
    | _ -> super#visit_stmt s 
end

let transform_ast t ast = ast
let transform_cfg t s = visit_stmt (new send_visitor t) s


