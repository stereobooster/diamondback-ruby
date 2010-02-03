
open Format
open Utils

type t = 
  | ClassType of class_annotation
  | MethodType of method_annotation list
  | ExprType of type_expr

and interface = require list * class_def list

and require = string

and class_def = 
  | ClassDef of class_annotation * class_elem list
  | ModuleDef of class_annotation * class_elem list
  | MetaClassDef of class_elem list

and class_elem = 
  | Alias of type_ident * type_ident
  | Class of class_def
  | Method of method_annotation list
  | IVar of string * type_expr
  | CVar of string * type_expr
  | GVar of string * type_expr
  | Const of string * type_expr

and class_annotation = type_ident * bounded_quantifier list * type_expr list

and method_annotation = type_ident * bounded_quantifier list * method_type

(* we need an explicit constructor here since the type is otherwise cyclic *)
and method_type = MethodSig of type_expr list * method_type option * type_expr

and type_expr = 
  | Type_Var of quant_var
  | Type_Ident of type_ident
  | Type_Object of object_type
  | Type_Union of type_expr list
  | Type_App of type_ident * type_expr list
  | Type_Tuple of type_expr list
  | Type_Dynamic
  | Type_Fixme
  | Type_Optional of type_expr
  | Type_Varargs of type_expr
  | Type_ParamList of type_expr list
  | Type_Named of string * type_expr (* Planned named arguments in interfaces *)

and type_ident = [
  | `ID_Var of [`Var_Constant] * string
  | `ID_UScope of string
  | `ID_Scope of type_ident * string
]

and object_type = field_type list * method_annotation list

and field_type = string * type_expr 

and bounded_quantifier = quant_var * type_expr option

and quant_var = 
  | QSelf
  | QVar of string
  | QParam of string

let format_quant_var ppf = function
  | QSelf -> pp_print_string ppf "self"
  | QVar s -> pp_print_string ppf s
  | QParam s -> fprintf ppf "^%s" s

let rec format_type_ident ppf = function
  | `ID_Var (`Var_Constant,s) -> pp_print_string ppf s
  | `ID_UScope s -> fprintf ppf "::%s" s
  | `ID_Scope(ti,s) -> fprintf ppf "%a::%s" format_type_ident ti s

let rec format_type_expr ppf = function
  | Type_Var qv -> format_quant_var ppf qv
  | Type_Ident ti -> format_type_ident ppf ti
  | Type_Object (fields,meths) -> 
      fprintf ppf "[%a,@ %a]"
        (format_comma_list format_field_type) fields
        (format_comma_list format_method_annotation) meths
  | Type_Union lst -> format_delim_list " or " format_type_expr ppf lst
  | Type_App(id,args) ->
      fprintf ppf "%a<%a>" format_type_ident id 
        (format_comma_list format_type_expr) args
  | Type_Tuple te_list ->  
      fprintf ppf "(%a)" (format_comma_list format_type_expr) te_list

  | Type_Dynamic -> pp_print_string ppf "?"
  | Type_Fixme -> pp_print_string ppf "!FIXME"
  | Type_Optional te -> fprintf ppf "?%a" format_type_expr te
  | Type_Varargs te -> fprintf ppf "*%a" format_type_expr te
  | Type_ParamList tlst ->
      fprintf ppf "^(%a)" (format_comma_list format_type_expr) tlst
  | Type_Named(name,te) ->
      fprintf ppf "%s: %a" (name) format_type_expr te

and format_field_type ppf (f,t) = fprintf ppf "%s: %a" f format_type_expr t

and format_method_type ppf (MethodSig(params,blk,ret)) = match params with
  | [x] -> 
      fprintf ppf "%a %a -> %a" format_type_expr x 
        format_type_block blk format_type_expr ret
  | lst -> 
      fprintf ppf "(@[<hov 0>%a@]) %a -> %a" (format_comma_list format_type_expr) lst
        format_type_block blk format_type_expr ret

and format_type_block ppf = function
  | None -> ()
  | Some mt -> fprintf ppf "{%a}" format_method_type mt

and format_method_annotation ppf = function
  | (name,[],mt) -> fprintf ppf "%a: %a" format_type_ident name format_method_type mt
  | (name,lst,mt) -> 
      fprintf ppf "%a<%a> : %a" format_type_ident name
        (format_comma_list format_bounded_quantifier) lst format_method_type mt

and format_bounded_quantifier ppf = function
  | qv, None -> format_quant_var ppf qv
  | qv, Some te -> fprintf ppf "%a < %a" format_quant_var qv format_type_expr te

let format_declared_subtypes ppf lst = 
  (format_comma_list (fun ppf x -> fprintf ppf "<= %a" format_type_expr x)) ppf lst

let format_class_annotation ppf : class_annotation -> unit = function
  | i,[],subs -> 
      fprintf ppf "%a %a" format_type_ident i 
        format_declared_subtypes subs
  | i,lst,subs -> 
      fprintf ppf "%a<%a> %a" format_type_ident i 
        (format_comma_list format_bounded_quantifier) lst
        format_declared_subtypes subs

let format_require ppf r = fprintf ppf "require \"%s\"@," r

let rec format_class_elem ppf = function
  | Alias (id1,id2) -> 
     fprintf ppf "alias %a %a" format_type_ident id1 format_type_ident id2
  | Class cls -> format_class_def ppf cls
  | Method [] -> assert false
  | Method [m] -> fprintf ppf "method %a" format_method_annotation m
  | Method (m::ms) -> 
      fprintf ppf "method %a@," format_method_annotation m;
      List.iter (fprintf ppf "   and %a" format_method_annotation) ms
  | IVar(fname,t)  
  | CVar(fname,t)
  | GVar(fname,t)
  | Const(fname,t) -> fprintf ppf "%s : %a" fname format_type_expr t

and format_class_def ppf = function
  | ClassDef(annot,elems) ->
      fprintf ppf "@[<v 2>class %a@,%a@]@,end@," format_class_annotation annot
        (format_break_list format_class_elem) elems
  | ModuleDef(annot,elems) ->
      fprintf ppf "@[<v 2>module %a@,%a@]@,end@," format_class_annotation annot
        (format_break_list format_class_elem) elems
  | MetaClassDef elems ->
      fprintf ppf "@[<v 2>metaclass@,%a@]@,end@," 
        (format_break_list format_class_elem) elems

let format_annotation ppf = function
  | ClassType c -> fprintf ppf "@[<v 0>##%% @[<h>%a@]@,@]" format_class_annotation c
  | MethodType mlst -> 
      fprintf ppf "@[<v 0>";
      List.iter (fprintf ppf "##%% @[<h>%a@]@," format_method_annotation) mlst;
      fprintf ppf "@]"
      
  | ExprType e -> fprintf ppf "@[<v 0>###%% @[<h>(%a)@]@]" format_type_expr e

let format_interface ppf (rlst,clst) =
  fprintf ppf "@[<v 0>";
  List.iter (format_require ppf) rlst;
  List.iter (format_class_def ppf) clst;
  fprintf ppf "@]"

let string_of_interface annot = 
  format_to_string format_interface annot

let string_of_quant_var = function
  | QSelf -> "self"
  | QParam s | QVar s -> s

let rec string_of_type_ident = function
  | `ID_Var(`Var_Constant,s) -> s
  | `ID_UScope (s) -> "::" ^ s
  | `ID_Scope(id, s) -> (string_of_type_ident id) ^ "::" ^ s

let string_of_annotation annot = 
  format_to_string format_annotation annot

let compare_class_annot c1 c2 = compare c1 c2
let compare_method_annot m1 m2 = compare m1 m2
let compare_expr_annot e1 e2 = compare e1 e2
let compare_alias s1 d1 s2 d2 = compare (s1, d1) (s2, d2)
let compare_require s1 s2 = compare s1 s2
let compare_interface = compare

let compare_annotation a1 a2 = match a1,a2 with
  | ClassType c1, ClassType c2 -> compare_class_annot c1 c2
  | MethodType m1, MethodType m2  -> compare_method_annot m1 m2
  | ExprType e1, ExprType e2 -> compare_expr_annot e1 e2
  | (ClassType _|MethodType _|ExprType _),
    (ClassType _|MethodType _|ExprType _) ->
      cmp_ctors a1 a2

let equal_annotation a1 a2 = (compare_annotation a1 a2) == 0

open Visitor

class type interface_visitor =
object
  method visit_interface : interface -> interface visitAction
  method visit_require : require -> require visitAction
  method visit_class_def : class_def -> class_def visitAction
  method visit_class_elem : class_elem -> class_elem visitAction
  method visit_class_annotation : class_annotation -> class_annotation visitAction
  method visit_method_annotation : method_annotation -> method_annotation visitAction
  method visit_type_ident : type_ident -> type_ident visitAction
  method visit_type_expr : type_expr -> type_expr visitAction
  method visit_quant_var : quant_var -> quant_var visitAction
  method visit_object_type : object_type -> object_type visitAction
  method visit_field_type : field_type -> field_type visitAction
  method visit_bounded_quantifier : bounded_quantifier -> bounded_quantifier visitAction
  method visit_method_type : method_type -> method_type visitAction
end

class default_interface_visitor : interface_visitor =
object(self)
  method visit_interface interface = DoChildren
  method visit_require require = DoChildren
  method visit_class_def def = DoChildren
  method visit_class_elem elem = DoChildren
  method visit_class_annotation annot = DoChildren
  method visit_method_annotation annot = DoChildren
  method visit_type_ident ident = DoChildren
  method visit_type_expr expr = DoChildren
  method visit_quant_var qvar = DoChildren
  method visit_object_type t = DoChildren
  method visit_field_type t = DoChildren
  method visit_bounded_quantifier q = DoChildren
  method visit_method_type t = DoChildren
end

let rec visit_interface vtor interface =
  visit vtor#visit_interface interface (visit_interface_children vtor)

and visit_interface_children vtor ((requires, class_defs) as interface) =
  let requires' = map_preserve List.map (visit_require vtor) requires in
  let class_defs' = map_preserve List.map (visit_class_def vtor) class_defs in
  if requires == requires' && class_defs == class_defs' then
    interface
  else
    (requires',class_defs')

and visit_require vtor (require : string) = require

and visit_class_def vtor class_def =
  visit vtor#visit_class_def class_def (visit_class_def_children vtor)

and visit_class_def_children vtor class_def =
  let recurse annot elems f =
    let annot' = visit_class_annotation vtor annot in
    let elems' = map_preserve List.map (visit_class_elem vtor) elems in
    if annot == annot' && elems == elems' then class_def else f annot' elems'
  in  
  match class_def with  
    | ClassDef(annot, elems) -> recurse annot elems (fun x y -> ClassDef(x,y))
    | ModuleDef(annot, elems) -> recurse annot elems (fun x y -> ModuleDef(x,y)) 
    | MetaClassDef(elems) ->
        let elems' = map_preserve List.map (visit_class_elem vtor) elems in
        if elems == elems' then class_def else MetaClassDef(elems')

and visit_class_annotation vtor annot =
  let visit_child vtor ((id, qlist, exprs) as annot) =
    let id' = visit_type_ident vtor id in
    let qlist' = map_preserve List.map (visit_bounded_quantifier vtor) qlist in
    let exprs' = map_preserve List.map (visit_type_expr vtor) exprs in
      if id == id' && qlist == qlist' && exprs == exprs' then
        annot
      else
        (id',qlist',exprs')
  in
  visit vtor#visit_class_annotation annot (visit_child vtor)

and visit_class_elem vtor t =
  visit vtor#visit_class_elem t (visit_class_elem_children vtor)

and visit_class_elem_children vtor elem =
  let visit_var vtor name expr f =
    let expr' = visit_type_expr vtor expr in
    if expr == expr' then elem else f name expr 
  in
  match elem with
  | Alias(id1, id2) ->
      let id1' = visit_type_ident vtor id1 in
      let id2' = visit_type_ident vtor id2 in
        if id1 == id1' && id2 == id2' then elem else Alias(id1',id2')
  | Class(def) ->
      let def' = visit_class_def vtor def in
        if def == def' then elem else Class(def')
  | Method(lst) ->
      let lst' = map_preserve List.map (visit_method_annotation vtor) lst in
        if lst == lst' then elem else Method(lst')
  | IVar(s,expr) -> visit_var vtor s expr (fun x y -> IVar(x, y))
  | CVar(s,expr) -> visit_var vtor s expr (fun x y -> CVar(x, y))
  | GVar(s,expr) -> visit_var vtor s expr (fun x y -> GVar(x, y))
  | Const(s,expr) -> visit_var vtor s expr (fun x y -> Const(x, y))
    
and visit_method_annotation vtor t = 
  visit vtor#visit_method_annotation t (visit_method_annotation_children vtor)

and visit_method_annotation_children vtor ((id, qlist, mt) as annot) =
  let id' = visit_type_ident vtor id in
  let qlist' = map_preserve List.map (visit_bounded_quantifier vtor) qlist in
  let mt' = visit_method_type vtor mt in
    if id == id' && qlist == qlist' && mt == mt' then annot else (id', qlist', mt')

and visit_type_ident vtor t = 
  visit vtor#visit_type_ident t (visit_type_ident_children vtor)

and visit_type_ident_children vtor tid = match tid with
  | `ID_Var _ | `ID_UScope _ -> tid
  | `ID_Scope(scope,name) ->
    let scope' = visit_type_ident vtor scope in
    if scope == scope' then tid else `ID_Scope (scope',name)

and visit_type_expr vtor t = 
  visit vtor#visit_type_expr t (visit_type_expr_children vtor)

and visit_type_expr_children vtor texpr = match texpr with
  | Type_Var qvar ->
    let qvar' = visit_quant_var vtor qvar in
    if qvar == qvar' then texpr else Type_Var qvar'
  | Type_Ident tid ->
    let tid' = visit_type_ident vtor tid in
    if tid == tid' then texpr else Type_Ident tid'
  | Type_Object obj ->
    let obj' = visit_object_type vtor obj in
    if obj == obj' then texpr else Type_Object obj'
  | Type_Union exprs ->
    let exprs' = map_preserve List.map (visit_type_expr vtor) exprs in
    if exprs == exprs' then texpr else Type_Union exprs'
  | Type_App(tid, exprs) ->
    let tid' = visit_type_ident vtor tid in
    let exprs' = map_preserve List.map (visit_type_expr vtor) exprs in
    if tid == tid' && exprs == exprs' then texpr else Type_App(tid',exprs')
  | Type_Tuple exprs ->
    let exprs' = map_preserve List.map (visit_type_expr vtor) exprs in
    if exprs == exprs' then texpr else Type_Tuple exprs'
  | Type_Dynamic | Type_Fixme -> texpr
  | Type_Optional expr ->
    let expr' = visit_type_expr vtor expr in
    if expr == expr' then texpr else Type_Optional expr'
  | Type_Varargs expr ->
    let expr' = visit_type_expr vtor expr in
    if expr == expr' then texpr else Type_Varargs expr'
  | Type_ParamList exprs ->
    let exprs' = map_preserve List.map (visit_type_expr vtor) exprs in
    if exprs == exprs' then texpr else Type_ParamList exprs'
  | Type_Named(name,expr) ->
    let expr' = visit_type_expr vtor expr in
    if expr == expr' then texpr else Type_Named(name,expr')

and visit_quant_var vtor t = 
  visit vtor#visit_quant_var t (visit_quant_var_children vtor)

and visit_quant_var_children vtor qvar = qvar

and visit_object_type vtor t = 
  visit vtor#visit_object_type t (visit_object_type_children vtor)

and visit_object_type_children vtor ((fs,ms) as obj) = 
  let fs' = map_preserve List.map (visit_field_type vtor) fs in
  let ms' = map_preserve List.map (visit_method_annotation vtor) ms in
  if fs == fs' && ms == ms' then obj else (fs',ms')

and visit_field_type vtor t = 
  visit vtor#visit_field_type t (visit_field_type_children vtor)

and visit_field_type_children vtor ((name,expr) as field) = 
  let expr' = visit_type_expr vtor expr in
  if expr == expr' then field else (name,expr')

and visit_bounded_quantifier vtor t = 
  visit vtor#visit_bounded_quantifier t (visit_bounded_quantifier_children vtor)

and visit_bounded_quantifier_children vtor ((qvar,expr_o) as bounded) = 
  let qvar' = visit_quant_var vtor qvar in
  let expr_o' = map_opt_preserve (visit_type_expr vtor) expr_o in
  if qvar == qvar' && expr_o == expr_o' then bounded else (qvar',expr_o')

and visit_method_type vtor t = 
  visit vtor#visit_method_type t (visit_method_type_children vtor)

and visit_method_type_children vtor mt = match mt with
  | MethodSig(exprs,mt_o,expr) ->
    let exprs' = map_preserve List.map (visit_type_expr vtor) exprs in
    let mt_o' = map_opt_preserve (visit_method_type vtor) mt_o in
    let expr' = visit_type_expr vtor expr in
    if exprs == exprs' && mt_o == mt_o' && expr == expr' then mt
    else MethodSig(exprs',mt_o',expr')
