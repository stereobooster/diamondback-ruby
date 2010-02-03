%{
  open Printf
  open Annotation

  let merge_quantifiers tvars cons pos = 
    let rec work acc tvars cons = match tvars, cons with
      | tvars, [] -> 
          let rest = List.map (fun x -> x,None) tvars in
            List.rev_append rest acc
      | [], (tv,b)::_ -> 
          Log.fatal (Log.of_loc pos) 
            "The type variable %a can not be used as a lower bound at this point."
            format_quant_var tv
      | tv::rest_tvars, _ -> 
          let bounds,rest_cons = 
            List.partition (fun (tv',bound) -> tv = tv') cons
          in
            match bounds with
              | [] -> work ((tv,None)::acc) rest_tvars rest_cons
              | [(_,t)] -> work ((tv,Some t)::acc) rest_tvars rest_cons
              | lst ->
                  let t = Type_Union (List.map snd lst) in
                    work ((tv,Some t)::acc) rest_tvars rest_cons
    in
    let final = work [] tvars cons in
      List.rev final

  let pragma key = match key with
    | "FIXME" -> Type_Fixme
    | _ -> failwith ("Unknown pragma keyword: "^key)

  let union x = match x with
    | [e] -> e
    | lst -> Type_Union(lst)

  let remove_name n = match n with
    | Type_Named(_, x) -> x
    | t -> t

  let apply_to_named f n = match n with
    | Type_Named(name, e) -> Type_Named(name, f e)
    | e -> f e

  let strip_quotes str = 
    let dbl_regexp = Str.regexp "\"\\(.+\\)\"" in
    if Str.string_match dbl_regexp str 0 then
      Str.matched_group 1 str
    else
      str

  let collapse_intersections (elem_list : Annotation.class_elem list) =
    (* build a method -> annotations hash table so we can combine all
     * associated annotations into a single Method type; returns a
     * list of elements with each method represented uniquely *)
    let rec build_annot_hash elem_list annot_hash (seen : string list)
        : Annotation.class_elem list =
      match elem_list with 
      | [] -> []
      | Method [(`ID_Var(`Var_Constant,name),_,_) as annot] :: rest -> 
          (* store annotations in a per-method-name hash table (fresh
           * hash table used for each collapsing attempt, so all
           * methods with the same name should share annotations) *)
          Hashtbl.add annot_hash name annot;
          (* if we've seen this method before, don't cons -- this will
           * give us a list of unique elements/method names when we're done *)
          if List.mem name seen then
            build_annot_hash rest annot_hash seen
          else
            (* return the whole class_elem to make types work out *)
            let h = List.hd elem_list in
            h::(build_annot_hash rest annot_hash (name::seen))
      | elem :: rest ->
          (* if this isn't a method, just hold on to it for later *)
          elem :: (build_annot_hash rest annot_hash seen)
    in
    (* once we've built the hash, iterate over the elements/unique
     * methods, converting parsed method defs into Annotation Method
     * types containing all associated annotations for each method *)
    let rec build_annots uniq_elem_list annot_hash =
      match uniq_elem_list with
      | [] -> []
      | Method [(`ID_Var(`Var_Constant,name),_,_)] :: rest ->
          (* hash contains all seen annotations for this method, but
             in reverse order *)
          let annots = Hashtbl.find_all annot_hash name in
          let m = Method (List.rev annots) in (* rev to correct order *)
          m :: build_annots rest annot_hash
      | elem :: rest -> (* just save all non-method elements *)
          elem :: build_annots rest annot_hash
    in
    let hash = Hashtbl.create 0 in
    let uniq = build_annot_hash elem_list hash [] in
    build_annots uniq hash

%}

%token T_EOF
%token <Lexing.position> K_CLASS K_METACLASS K_MODULE K_INTERFACE
%token <Lexing.position> K_ALIAS K_REQUIRE K_END
%token <Lexing.position> K_OR K_SELF

%token <Lexing.position> T_BEGIN_LINE T_SEMICOLON
%token <Lexing.position> T_COLON T_DOUBLE_COLON T_DOT 
%token <Lexing.position> T_STAR T_QUESTION 
%token <Lexing.position> T_CARROT T_BANG

%token <Lexing.position> T_RARROW 
%token <Lexing.position> T_LPAREN T_RPAREN 
%token <Lexing.position> T_LESS T_GREATER T_COMMA
%token <Lexing.position> T_LBRACKET T_RBRACKET
%token <Lexing.position> T_LBRACE T_RBRACE

%token <Lexing.position> T_SUBTYPE

%token <Lexing.position * string> T_STRING
%token <Lexing.position * string> T_IVAR T_CVAR T_GVAR
%token <Lexing.position * string> T_CONST_ID T_TYPE_ID
%token <Lexing.position * string> T_METHOD_NAME

%token <Lexing.position>

%left T_COMMA
%right T_RARROW 
%left K_OR

%start e_method e_class e_expr interface_file
%type <Annotation.t> e_method e_class e_expr
%type <Annotation.interface> interface_file

%%

interface_file:
  | require_list class_def_list T_EOF { ($1, $2) }

require_list:
  | { [] }
  | require_stmt require_list {$1::$2}

require_stmt:
  | K_REQUIRE T_STRING { strip_quotes (snd $2) }

class_def_list:
  | { [] } 
  | class_def class_def_list {$1::$2}

class_def:
  | K_METACLASS class_elem_list K_END {
    MetaClassDef(collapse_intersections $2) }
  | K_CLASS class_annotation class_elem_list K_END {
    ClassDef($2, collapse_intersections $3) }
  | K_MODULE class_annotation class_elem_list K_END { ModuleDef($2,$3) } 

class_elem_list:
  | { [] }
  | class_elem class_elem_list {$1::$2}

alias_name:
  | relative_method_name {`ID_Var(`Var_Constant,$1)}

class_elem:
  | K_ALIAS alias_name alias_name { Alias ($2, $3) }
  | class_def { Class ($1) }
  | method_type { Method [$1] }
  | field_sig { $1 }
  | const_expr { $1 }

field_sig:
  | T_IVAR T_COLON type_expr { IVar(snd $1,$3) }
  | T_CVAR T_COLON type_expr { CVar(snd $1,$3) }
  | T_GVAR T_COLON type_expr { GVar(snd $1,$3) }

e_expr:
  | type_expr T_EOF {ExprType $1}

e_method:
  | method_annotation_list T_EOF { MethodType $1 }

e_class:
  | class_annotation T_EOF     { ClassType $1 }

method_start:
  | T_BEGIN_LINE { () }
  | { () }

method_annotation_list: 
  | method_start const_method_type { [$2] }
  | method_start const_method_type method_annotation_list { $2::$3 }

/* Since "Constants" can appear as either values, or method names, 
   we need to inline these cases explicitly to make the parser happy */ 
const_expr:
  | T_CONST_ID T_COLON type_expr { Const(snd $1,$3) }
  | T_CONST_ID T_LESS type_id_list T_GREATER constraint_list T_COLON method_sig 
      { Method[(`ID_Var(`Var_Constant,snd $1), (merge_quantifiers $3 $5 $2), $7)] }
  | T_CONST_ID T_COLON method_sig {Method[(`ID_Var(`Var_Constant,snd $1),[],$3)]}

const_method_type:
  | method_name T_LESS type_id_list T_GREATER constraint_list T_COLON method_sig 
      { $1, (merge_quantifiers $3 $5 $2), $7 }
  | method_name T_COLON method_sig
      { $1, [], $3 }
  | T_CONST_ID T_LESS type_id_list T_GREATER constraint_list T_COLON method_sig 
      { (`ID_Var(`Var_Constant,snd $1), (merge_quantifiers $3 $5 $2), $7) }
  | T_CONST_ID T_COLON method_sig {(`ID_Var(`Var_Constant,snd $1),[],$3)}

line_start:
  | { () }
  | T_BEGIN_LINE { () }

method_type:
  | method_name T_LESS type_id_list T_GREATER constraint_list T_COLON method_sig 
      { $1, (merge_quantifiers $3 $5 $2), $7 }
  | method_name T_COLON method_sig
      { $1, [], $3 }

relative_method_name:
  | T_STRING { snd $1 }
  | T_METHOD_NAME { snd $1 }
  | T_TYPE_ID { snd $1 }

method_name:
  | relative_method_name {`ID_Var(`Var_Constant,$1)}

method_sig:
  | T_LPAREN T_RPAREN block T_RARROW type_expr { MethodSig([],$3,$5) }
  | /*named_*/type_expr block T_RARROW /*named_*/ type_expr
      { match $1 with
          | Type_Tuple lst -> MethodSig(lst,$2,$4) 
          | t -> MethodSig([t],$2,$4)
      }

block:
  | { None }
  | T_LBRACE method_sig T_RBRACE {Some $2}

type_id_list:
  | type_var { [$1] }
  | type_var T_COMMA type_id_list { $1::$3 }

type_var:
  | T_TYPE_ID { QVar (snd $1) }
  | K_SELF { QSelf }
  | T_CARROT T_TYPE_ID { QParam (snd $2) }

type_ident:
  | T_CONST_ID { fst $1, `ID_Var( `Var_Constant, snd $1) }
  | T_DOUBLE_COLON T_CONST_ID { fst $2, `ID_UScope (snd $2) }
  | type_ident T_DOUBLE_COLON T_CONST_ID { fst $3, `ID_Scope(snd $1, snd $3) }

type_expr:
  | or_type_list { union $1 }

or_type_list:
  | single_type_expr { [$1] }
  | single_type_expr K_OR or_type_list { $1::$3 }

single_type_expr:
  | type_var { Type_Var $1 }
  | type_ident { Type_Ident (snd $1) }
  | tuple { $1 }
  | T_DOT T_QUESTION { Type_Dynamic }
  | T_QUESTION single_type_expr { Type_Optional $2 }
  | T_STAR single_type_expr { Type_Varargs $2 }
  | T_LBRACKET field_or_method_list T_RBRACKET { Type_Object $2 }
  | type_ident T_LESS type_expr_comma_list T_GREATER
      { Type_App(snd $1, $3) }
  | T_BANG T_CONST_ID { pragma (snd $2) }
  | T_CARROT T_LPAREN type_expr_comma_list T_RPAREN
      { Type_ParamList $3 }

tuple:
  | T_LPAREN type_expr_comma_list T_RPAREN 
      { match $2 with
          | [] -> assert false
          | [x] -> x
          | lst -> Type_Tuple lst }

named_type_expr:
  | named_or_type_list { union $1 }

named_or_type_list:
  | single_named_type_expr { [$1] }
  | single_named_type_expr K_OR named_or_type_list { $1::$3 }

single_named_type_expr:
  | single_type_expr { $1 }
  | T_TYPE_ID T_COLON single_type_expr { Type_Named (snd $1, $3) }

type_expr_comma_list:
  | named_type_expr { [$1] }
  | named_type_expr T_COMMA type_expr_comma_list {$1::$3}

declared_subtypes:
  | { [] }
  | T_SUBTYPE type_expr_comma_list { $2 }

class_decl:
  | type_ident T_LESS type_id_list T_GREATER { fst $1, snd $1, $3 }
  | type_ident { fst $1, snd $1, [] }

class_annotation:
  | line_start class_decl declared_subtypes constraint_list
      { let pos,name,vars = $2 in
        let subs = $3 in
        let cons = $4 in
        let qlist = merge_quantifiers vars cons pos in
          (name,qlist,subs)}

  | line_start error /*T_NEWLINE*/
      { Log.fatal (Log.empty) "parse error in annotation" }

constraint_list:
  | { [] }
  | T_SEMICOLON bounded_quantifier_list { $2 }

bounded_quantifier_list:
  | bounded_quantifier {[$1]}
  | bounded_quantifier T_COMMA bounded_quantifier_list {$1::$3}

bounded_quantifier:
  | type_var T_SUBTYPE type_expr { $1, $3 }

field_type: 
  | T_IVAR T_COLON type_expr { (snd $1), $3 }

field_or_method_nonempty_list:
  | field_type { [$1],[] }
  | method_type { [],[$1] }
  | field_type T_COMMA field_or_method_nonempty_list 
      { let f,m = $3 in ($1::f),m }
  | method_type T_COMMA field_or_method_nonempty_list 
      { let f,m = $3 in f, ($1::m) }

field_or_method_list:
  | { [],[] }
  | field_or_method_nonempty_list {$1}
