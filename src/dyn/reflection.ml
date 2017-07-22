
open Printf
open Dynamic
open Yaml
open Utils
open Dyn_builder

module Domain = YString
module CoDomain = YString

let name = "reflection"

let instrument_ast e = e
let transform_ast t e = e 

open Cfg
module C = Cfg.Abbr

let refl_watch meth = 
  {src_msg = `ID_MethodName meth;
   dst_target = C.access_path ["DRuby";"Profile";"Reflection"];
   dst_msg = `ID_MethodName "watch";
  }

let instrument_cfg s = 
  let trans = [
    refl_watch "instance_variable_get";
    refl_watch "class_variable_get";
    refl_watch "const_get";
    refl_watch "instance_variable_set";
    refl_watch "class_variable_set";
    refl_watch "const_set";

    refl_watch "attr";
    refl_watch "attr_accessor";
    refl_watch "attr_writer";
    refl_watch "attr_reader";
  ] in Cfg.visit_stmt (new watch_visitor ~rec_lit_str_and_atom:false trans) s

module Base = struct
  module Domain = Domain
  module Codomain = CoDomain
  let preamble = []
  let when_guard str = C.str str
end

class refl_visitor yaml_map = 
object(self)
  inherit default_visitor
  method visit_stmt s = match s.snode with
    | MethodCall(lhs,mc) -> begin match mc with
        | {mc_msg=`ID_MethodName ("instance_variable_get"|"class_variable_get");
           mc_args=(`Lit_String vname | `Lit_Atom vname)::_} ->
            let body = C.next ~v:(C.var vname) s.pos in
            let cb = CB_Block([],body) in
            let s' = C.mcall ?lhs ?targ:mc.mc_target (`ID_MethodName "instance_eval") [] ~cb s.pos in
              Visitor.ChangeTo s'

        | {mc_msg=`ID_MethodName ("instance_variable_get"|"class_variable_get" as get);
           mc_args=(#expr as arg)::_} ->
            let module M = struct
              include Base
              let case_guard = arg
              let else_body = dead_method get [(arg :> star_expr)] s.pos
              let when_body str = 
                let body = C.next ~v:(C.var str) s.pos in
                let cb = CB_Block([],body) in
                  C.mcall ?lhs ?targ:mc.mc_target (`ID_MethodName "instance_eval") [] ~cb s.pos
            end in let module T = CaseTransformer(M) in
              T.transform yaml_map get s.pos

        | {mc_msg=`ID_MethodName ("instance_variable_set"|"class_variable_set");
           mc_args=[(`Lit_String vname | `Lit_Atom vname);(#star_expr as rhs)]} ->
            let body = C.seq [C.assign (C.var vname) (rhs:>tuple_expr) s.pos;
                                  C.next ~v:(C.var vname) s.pos] s.pos 
            in
            let cb = CB_Block([],body) in
            let s' = C.mcall ?lhs ?targ:mc.mc_target (`ID_MethodName "instance_eval") [] ~cb s.pos in
              Visitor.ChangeTo s'
                
        | {mc_msg=`ID_MethodName ("instance_variable_set"|"class_variable_set" as set);
           mc_args=[(#expr as arg);(#star_expr as rhs)]} ->
            let module M = struct
              include Base
              let case_guard = arg
              let else_body = dead_method set [(arg :> star_expr)] s.pos
              let when_body str = 
                let body = C.seq [C.assign (C.var str) (rhs:>tuple_expr) s.pos;
                                  C.next ~v:(C.var str) s.pos] s.pos 
                in
                let cb = CB_Block([],body) in
                  C.mcall ?lhs ?targ:mc.mc_target (`ID_MethodName "instance_eval") [] ~cb s.pos
            end in let module T = CaseTransformer(M) in
              T.transform yaml_map set s.pos

        | {mc_msg=`ID_MethodName ("const_get"|"const_set"); mc_args=(`Lit_Atom _)::_} ->
            (* already uses a literal, no need to transform *)
            Visitor.SkipChildren

        | {mc_msg=`ID_MethodName ("const_set"|"const_get" as const);mc_args=(#expr as arg)::rest} ->
            let tmp = C.local "__druby_profiled_const" in
            let to_s = C.mcall ~lhs:tmp ~targ:arg (`ID_MethodName "to_s") [] s.pos in
            let module M = struct
              include Base
              let preamble = [to_s]
              let case_guard = arg
              let else_body = dead_method const [(arg :> star_expr)] s.pos
              let when_body str = 
                mkstmt (MethodCall(lhs,{mc with mc_args=(C.atom str)::rest})) s.pos
            end in let module T = CaseTransformer(M) in
              T.transform yaml_map const s.pos

        | {mc_target=targ;mc_args=(`Lit_String _ | `Lit_Atom _)::rest; mc_cb=None;
           mc_msg=`ID_MethodName ("attr"|"attr_accessor"|"attr_reader"|"attr_writer")
          } -> Visitor.DoChildren

        | {mc_target=targ;mc_args=(#expr as arg)::rest; mc_cb=None;
           mc_msg=`ID_MethodName ("attr"|"attr_accessor"|"attr_reader"|"attr_writer" as attr)
          } ->
            let module M = struct
              include Base
              let case_guard = arg
              let else_body = dead_method attr [arg] s.pos
              let when_body str = 
                C.mcall ?lhs ?targ (`ID_MethodName attr) ((C.str str)::rest) s.pos
            end in let module T = CaseTransformer(M) in
              T.transform yaml_map attr s.pos

        | _ -> Visitor.DoChildren
      end
    | _ -> Visitor.DoChildren
end

let transform_cfg t s = visit_stmt (new refl_visitor t) s
