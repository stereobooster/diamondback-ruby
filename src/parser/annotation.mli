
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
  (* TODO Constants, Instance vars, Class instance vars, Class vars, 
  * attr_... ? *)

and class_annotation = type_ident * bounded_quantifier list * type_expr list

and method_annotation = type_ident * bounded_quantifier list * method_type

and method_type =
    MethodSig of type_expr list * method_type option * type_expr

and type_expr =
    Type_Var of quant_var
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
  | Type_Named of string * type_expr

and type_ident = [
    `ID_Var of [`Var_Constant] * string
  | `ID_UScope of string
  | `ID_Scope of type_ident * string
]

and object_type = field_type list * method_annotation list

and field_type = string * type_expr

and bounded_quantifier = quant_var * type_expr option

and quant_var = QSelf | QVar of string | QParam of string

(* pretty printers *)
val format_quant_var : Format.formatter -> quant_var -> unit
val format_type_ident : Format.formatter -> type_ident -> unit
val format_type_expr : Format.formatter -> type_expr -> unit
val format_field_type : Format.formatter -> field_type -> unit
val format_method_type : Format.formatter -> method_type -> unit
val format_type_block : Format.formatter -> method_type option -> unit
val format_method_annotation : Format.formatter -> method_annotation -> unit
val format_bounded_quantifier : Format.formatter -> bounded_quantifier -> unit
val format_class_annotation : Format.formatter -> class_annotation -> unit
val format_interface : Format.formatter -> interface -> unit
val format_annotation : Format.formatter -> t -> unit

val string_of_quant_var : quant_var -> string
val string_of_interface : interface -> string
val string_of_type_ident : type_ident -> string
val string_of_annotation : t -> string

val compare_interface : interface -> interface -> int
val compare_class_annot : class_annotation -> class_annotation -> int
val compare_method_annot : method_annotation list -> method_annotation list -> int
val compare_expr_annot : type_expr -> type_expr -> int
val compare_alias : type_ident -> type_ident -> type_ident -> type_ident -> int
val compare_require : string -> string -> int
val compare_annotation : t -> t -> int
val equal_annotation : t -> t -> bool


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

class default_interface_visitor : interface_visitor

val visit_interface : interface_visitor -> interface -> interface 
val visit_require : interface_visitor -> require -> require 
val visit_class_def : interface_visitor -> class_def -> class_def 
val visit_class_elem : interface_visitor -> class_elem -> class_elem 
val visit_class_annotation : interface_visitor -> class_annotation -> class_annotation 
val visit_type_ident : interface_visitor -> type_ident -> type_ident 
val visit_type_expr : interface_visitor -> type_expr -> type_expr 
val visit_quant_var : interface_visitor -> quant_var -> quant_var 
val visit_object_type : interface_visitor -> object_type -> object_type 
val visit_field_type : interface_visitor -> field_type -> field_type 
val visit_bounded_quantifier : interface_visitor -> bounded_quantifier -> bounded_quantifier 

