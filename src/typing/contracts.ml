
open Cfg
open Cfg_printer.CodePrinter
open Utils
open Format
open Config
open Annotation

let is_vararg = function 
  | Type_Varargs _ -> true
  | _ -> false

let is_optional = function 
  | Type_Optional _ -> true
  | _ -> false

let emit_pos ppf pos = 
  fprintf ppf "DRuby::Contract::Origin.new('%s',%d,nil)"
    pos.Lexing.pos_fname pos.Lexing.pos_lnum

let rec emit_id ppf ident = 
  let name = format_to_string format_type_ident ident in
    if name = "::Boolean"
    then fprintf ppf "DRuby::Contract::UnionType.new([DRuby::Contract::ClassType.new('::TrueClass'),Contract::ClassType.new('::FalseClass')])"
    else fprintf ppf "DRuby::Contract::ClassType.new('%s')" name


let rec emit_type_expr pos ppf e = match e with
  | Type_Ident ident -> emit_id ppf ident

  | Type_Union lst ->
      fprintf ppf "DRuby::Contract::UnionType.new([%a])"
        (format_comma_list (emit_type_expr pos)) lst

  | Type_Object(fields,methods) ->
      fprintf ppf "DRuby::Contract::ObjectType.new({%a},{%a})"
        (format_comma_list (emit_field pos)) fields
        (format_comma_list (emit_obj_method pos)) methods
      
  | Type_Var v -> 
      fprintf ppf "DRuby::Contract::PolyVar.new(:%s)" 
        (Annotation.string_of_quant_var v)
      
  | Type_Varargs _ | Type_Optional _
  | Type_ParamList _
  | Type_App _
  | Type_Tuple _ ->
      Log.fixme "contract emit unsupported: %a" format_type_expr e;
      fprintf ppf "DRuby::Contract::ClassType.new('Object')"

  | Type_Dynamic
  | Type_Fixme -> 
      Log.fixme "contract emit unsupported";
      fprintf ppf "DRuby::Contract::ClassType.new('Object')"

and emit_field pos ppf (name,typ) = 
  fprintf ppf ":%s => (%a)" name (emit_type_expr pos) typ

and emit_obj_method pos ppf ((name,_,_) as mt) = 
  let mname = match name with
    | TIdent_Relative s -> s
    | TIdent_Absolute s -> s
    | TIdent_Scoped(_,s) ->
        Log.fixme "don't yet support dynamically checking class methods";
        s
  in
    fprintf ppf ":%s => %a" mname (emit_annot_method pos mname) mt

and emit_params pos ppf pl = 
  let emit ppf = function
    | Type_Optional e | Type_Varargs e -> emit_type_expr pos ppf e
    | e -> emit_type_expr pos ppf e
  in
  let varargs,rest = List.partition is_vararg pl in
  let opts,reqs = List.partition is_optional rest in
    fprintf ppf "DRuby::Contract::Params.new([%a]"
      (format_comma_list (emit_type_expr pos)) reqs;
    fprintf ppf ",[%a]" (format_comma_list emit) opts;
    begin match varargs with
      | [] -> ()
      | [v] -> fprintf ppf ",%a" emit v
      | _ -> assert false
    end;
    fprintf ppf ")"
      
and emit_block pos ppf (MethodSig(args,blk,ret)) = 
  assert(blk = None);
  fprintf ppf "DRuby::Contract::Block.new(%a,%a)"
    (emit_params pos) args
    (emit_type_expr pos) ret

and emit_block_or_nil pos ppf = function
  | None -> pp_print_string ppf "nil"
  | Some x -> emit_block pos ppf x

and emit_method_args pos ppf (MethodSig(args,blk,ret)) =
  fprintf ppf "%a,%a,%a"
    (emit_params pos) args
    (emit_block_or_nil pos) blk
    (emit_type_expr pos) ret

and emit_binder pos ppf (var,bound) = 
  let vname = Annotation.string_of_quant_var var in
  let emit_bound ppf = function
    | None -> pp_print_string ppf "nil" 
    | Some e -> emit_type_expr pos ppf e
  in
    fprintf ppf "[:%s,%a]" vname emit_bound bound


and emit_annot_method pos name ppf (_,binders,methsig) =
  let mono ppf () = fprintf ppf "DRuby::Contract::MonoMethod.new(:%s,%a,%a)" 
    name (emit_method_args pos) methsig emit_pos pos
  in
    match binders with
      | [] -> mono ppf ()
      | lst ->
          fprintf ppf "DRuby::Contract::PolyMethod.new([%a],%a,%a)" 
            (format_comma_list (emit_binder pos)) binders
            mono () emit_pos pos


and emit_annot_inter pos name ppf alst = 
  let emit_one ppf m = fprintf ppf "%a" (emit_annot_method pos name) m in
    fprintf ppf "DRuby::Contract::InterMethod.new(:%s,[%a],%a)\n" 
      name
      (format_comma_list emit_one) alst
      emit_pos pos

and emit_method pos name ppf lst =
  match lst with
    | [] -> assert false
    | [m] -> fprintf ppf "%a\n" (emit_annot_method pos name) m
    | lst -> fprintf ppf "%a\n" (emit_annot_inter pos name) lst


let out_ppf_ref = ref None

let open_output_printer () = match !out_ppf_ref with
  | None ->
      begin try 
        let oc = open_out "casts.rb" in
        let ppf = formatter_of_out_channel oc in
          out_ppf_ref := Some ppf;
          fprintf ppf "require 'druby'\n";
          fprintf ppf "require 'druby/contract/signature'\n\n";
          ppf
      with _ -> err_formatter
      end
  | Some ppf -> ppf


let emit_contract clz ann_lst pos = 
  let out_ppf = open_output_printer() in
  let name = match ann_lst with
    | [] -> assert false
    | (name,_,_)::_ -> format_to_string format_type_ident name
  in
  let clz = 
    if substr "::" clz 
    then String.sub clz 2 (String.length clz - 2)
    else clz
  in
    fprintf out_ppf "sig = %a\n" (emit_method pos name) ann_lst;
    fprintf out_ppf "DRuby::Contract::Registry.register('%s',:'%s',sig)\n\n" clz name;
    pp_print_flush out_ppf ()

let emit_method_contract clz annot pos = match annot with
  | MethodType [(_,[],_)] -> () (* skip non-polymorphic, non-intersection types *)
  | MethodType lst -> emit_contract clz lst pos
      
  | _ -> Log.fatal (Log.of_loc pos) "wrong kind of annotation for method"

let flush_ppf () = match !out_ppf_ref with
  | None -> ()
  | Some ppf -> pp_print_flush ppf ()

let () = at_exit flush_ppf
               
    
(*
and annot_method = {
  annot_meth_name : def_name;
  annot_meth_binders : TypeAnnot_helper.binder list;
  annot_meth_typ : annot_func;
  annot_meth_block : annot_func option;
  annot_meth_cons : annot_cons list;
}
  ()
*)  
