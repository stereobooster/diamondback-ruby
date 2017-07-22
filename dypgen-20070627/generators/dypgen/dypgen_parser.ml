
module Parser_parameters_module =
struct

let token_name_EOF = 2
let token_name_LIDENT = 3
let token_name_UIDENT = 4
let token_name_PATTERN = 5
let token_name_OCAML_TYPE = 6
let token_name_OCAML_CODE = 7
let token_name_KWD_NON_TERMINAL = 8
let token_name_KWD_TYPE = 9
let token_name_KWD_FOR = 10
let token_name_KWD_CONSTRUCTOR = 11
let token_name_KWD_MERGE = 12
let token_name_KWD_MLI = 13
let token_name_KWD_RELATION = 14
let token_name_KWD_START = 15
let token_name_KWD_TOKEN = 16
let token_name_EQUAL = 17
let token_name_GREATER = 18
let token_name_LESS = 19
let token_name_BAR = 20
let token_name_RBRACE = 21
let token_name_LBRACE = 22
let token_name_PERCENTPERCENT = 23
let token_name_COLON = 24
let token_name_SEMI = 25
let token_name_COMMA = 26
let token_name_RPAREN = 27
let token_name_LPAREN = 28
let token_nb = 29
let token_name_array = [|"dummy_token";"token_epsilon";"EOF";"LIDENT";"UIDENT";"PATTERN";"OCAML_TYPE";"OCAML_CODE";"KWD_NON_TERMINAL";"KWD_TYPE";"KWD_FOR";"KWD_CONSTRUCTOR";"KWD_MERGE";"KWD_MLI";"KWD_RELATION";"KWD_START";"KWD_TOKEN";"EQUAL";"GREATER";"LESS";"BAR";"RBRACE";"LBRACE";"PERCENTPERCENT";"COLON";"SEMI";"COMMA";"RPAREN";"LPAREN"|]
let str_token_name t = token_name_array.(t)
let entry_def = 1
let grammar = 2
let lident_list = 3
let literal = 4
let literal_list = 5
let main = 6
let nested = 7
let opt_bar = 8
let opt_par_act = 9
let opt_pattern = 10
let optional_code = 11
let optional_mli = 12
let optional_trailer = 13
let parser_param_info = 14
let parser_param_infos = 15
let priority = 16
let relation = 17
let relation_list = 18
let rhs = 19
let rhs_list = 20
let token_list = 21
let uident_list = 22
let nt_names = [|"0";"entry_def";"grammar";"lident_list";"literal";"literal_list";"main";"nested";"opt_bar";"opt_par_act";"opt_pattern";"optional_code";"optional_mli";"optional_trailer";"parser_param_info";"parser_param_infos";"priority";"relation";"relation_list";"rhs";"rhs_list";"token_list";"uident_list"|]
let entry_points = [(main,1)]
let priority_names = [|"0"|]let merge_warning = false
let undef_nt = true
end

module P = Dyp.Make_dyp(Parser_parameters_module)
open Parser_parameters_module
open P
open P.Parser_PAR
open Dyp
type priority = Dyp.priority
let priority_data, default_priority =
  Dyp.insert_priority Dyp.empty_priority_data "default_priority"

type token = | EOF | LIDENT of (string * (int * int * int)) | UIDENT of (string * (int * int * int)) | PATTERN of (string * (int * int)) | OCAML_TYPE of (string) | OCAML_CODE of (string * (int * int)) | KWD_NON_TERMINAL | KWD_TYPE | KWD_FOR | KWD_CONSTRUCTOR | KWD_MERGE | KWD_MLI | KWD_RELATION | KWD_START | KWD_TOKEN | EQUAL | GREATER | LESS | BAR | RBRACE | LBRACE | PERCENTPERCENT | COLON | SEMI | COMMA | RPAREN | LPAREN
let get_name t = match t with | EOF -> token_name_EOF | LIDENT _ -> token_name_LIDENT | UIDENT _ -> token_name_UIDENT | PATTERN _ -> token_name_PATTERN | OCAML_TYPE _ -> token_name_OCAML_TYPE | OCAML_CODE _ -> token_name_OCAML_CODE | KWD_NON_TERMINAL -> token_name_KWD_NON_TERMINAL | KWD_TYPE -> token_name_KWD_TYPE | KWD_FOR -> token_name_KWD_FOR | KWD_CONSTRUCTOR -> token_name_KWD_CONSTRUCTOR | KWD_MERGE -> token_name_KWD_MERGE | KWD_MLI -> token_name_KWD_MLI | KWD_RELATION -> token_name_KWD_RELATION | KWD_START -> token_name_KWD_START | KWD_TOKEN -> token_name_KWD_TOKEN | EQUAL -> token_name_EQUAL | GREATER -> token_name_GREATER | LESS -> token_name_LESS | BAR -> token_name_BAR | RBRACE -> token_name_RBRACE | LBRACE -> token_name_LBRACE | PERCENTPERCENT -> token_name_PERCENTPERCENT | COLON -> token_name_COLON | SEMI -> token_name_SEMI | COMMA -> token_name_COMMA | RPAREN -> token_name_RPAREN | LPAREN -> token_name_LPAREN
let str_token t = match t with
 | EOF -> "EOF" | LIDENT _ -> "LIDENT" | UIDENT _ -> "UIDENT" | PATTERN _ -> "PATTERN" | OCAML_TYPE s -> s | OCAML_CODE _ -> "OCAML_CODE" | KWD_NON_TERMINAL -> "KWD_NON_TERMINAL" | KWD_TYPE -> "KWD_TYPE" | KWD_FOR -> "KWD_FOR" | KWD_CONSTRUCTOR -> "KWD_CONSTRUCTOR" | KWD_MERGE -> "KWD_MERGE" | KWD_MLI -> "KWD_MLI" | KWD_RELATION -> "KWD_RELATION" | KWD_START -> "KWD_START" | KWD_TOKEN -> "KWD_TOKEN" | EQUAL -> "EQUAL" | GREATER -> "GREATER" | LESS -> "LESS" | BAR -> "BAR" | RBRACE -> "RBRACE" | LBRACE -> "LBRACE" | PERCENTPERCENT -> "PERCENTPERCENT" | COLON -> "COLON" | SEMI -> "SEMI" | COMMA -> "COMMA" | RPAREN -> "RPAREN" | LPAREN -> "LPAREN"
type ( 'entry_def, 'grammar, 'lident_list, 'literal, 'literal_list, 'nested, 'opt_bar, 'opt_par_act, 'opt_pattern, 'optional_code, 'optional_mli, 'optional_trailer, 'parser_param_info, 'parser_param_infos, 'priority, 'relation, 'relation_list, 'rhs, 'rhs_list, 'token_list, 'uident_list) obj =
  | Obj_BAR
  | Obj_COLON
  | Obj_COMMA
  | Obj_EOF
  | Obj_EQUAL
  | Obj_GREATER
  | Obj_KWD_CONSTRUCTOR
  | Obj_KWD_FOR
  | Obj_KWD_MERGE
  | Obj_KWD_MLI
  | Obj_KWD_NON_TERMINAL
  | Obj_KWD_RELATION
  | Obj_KWD_START
  | Obj_KWD_TOKEN
  | Obj_KWD_TYPE
  | Obj_LBRACE
  | Obj_LESS
  | Obj_LIDENT of (string * (int * int * int))
  | Obj_LPAREN
  | Obj_OCAML_CODE of (string * (int * int))
  | Obj_OCAML_TYPE of (string)
  | Obj_PATTERN of (string * (int * int))
  | Obj_PERCENTPERCENT
  | Obj_RBRACE
  | Obj_RPAREN
  | Obj_SEMI
  | Obj_UIDENT of (string * (int * int * int))
  | Obj_entry_def of 'entry_def
  | Obj_grammar of 'grammar
  | Obj_lident_list of 'lident_list
  | Obj_literal of 'literal
  | Obj_literal_list of 'literal_list
  | Obj_main of Parse_tree.obj
  | Obj_nested of 'nested
  | Obj_opt_bar of 'opt_bar
  | Obj_opt_par_act of 'opt_par_act
  | Obj_opt_pattern of 'opt_pattern
  | Obj_optional_code of 'optional_code
  | Obj_optional_mli of 'optional_mli
  | Obj_optional_trailer of 'optional_trailer
  | Obj_parser_param_info of 'parser_param_info
  | Obj_parser_param_infos of 'parser_param_infos
  | Obj_priority of 'priority
  | Obj_relation of 'relation
  | Obj_relation_list of 'relation_list
  | Obj_rhs of 'rhs
  | Obj_rhs_list of 'rhs_list
  | Obj_token_list of 'token_list
  | Obj_uident_list of 'uident_list
type data = Data_void
let __dypgen_get_value t = match t with
  | EOF -> Obj_EOF
  | LIDENT x -> Obj_LIDENT x
  | UIDENT x -> Obj_UIDENT x
  | PATTERN x -> Obj_PATTERN x
  | OCAML_TYPE x -> Obj_OCAML_TYPE x
  | OCAML_CODE x -> Obj_OCAML_CODE x
  | KWD_NON_TERMINAL -> Obj_KWD_NON_TERMINAL
  | KWD_TYPE -> Obj_KWD_TYPE
  | KWD_FOR -> Obj_KWD_FOR
  | KWD_CONSTRUCTOR -> Obj_KWD_CONSTRUCTOR
  | KWD_MERGE -> Obj_KWD_MERGE
  | KWD_MLI -> Obj_KWD_MLI
  | KWD_RELATION -> Obj_KWD_RELATION
  | KWD_START -> Obj_KWD_START
  | KWD_TOKEN -> Obj_KWD_TOKEN
  | EQUAL -> Obj_EQUAL
  | GREATER -> Obj_GREATER
  | LESS -> Obj_LESS
  | BAR -> Obj_BAR
  | RBRACE -> Obj_RBRACE
  | LBRACE -> Obj_LBRACE
  | PERCENTPERCENT -> Obj_PERCENTPERCENT
  | COLON -> Obj_COLON
  | SEMI -> Obj_SEMI
  | COMMA -> Obj_COMMA
  | RPAREN -> Obj_RPAREN
  | LPAREN -> Obj_LPAREN

let merge ol _ = ol

open Printf
open Parse_tree

let () = dypgen_verbose := 0

let empty_ppi = {
  token_list = [];
  relation = [];
  start = [];
  generic_merge = [];
  cons = [];
  additional_cons = [];
  nt_type = [];
  single_nt = [] }

let rapf_list = [
((opt_pattern,[Ter token_name_PATTERN],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_PATTERN _1] -> Obj_opt_pattern ( (fst _1,snd _1) ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((opt_pattern,[],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [] -> Obj_opt_pattern ( ("_",(0,0)) ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((opt_par_act,[Ter token_name_OCAML_CODE;Non_ter (opt_pattern,No_priority )],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_OCAML_CODE _1;Obj_opt_pattern _2] -> Obj_opt_par_act ( (_1,[fst _2,"_",snd _2]) ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((opt_par_act,[],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [] -> Obj_opt_par_act ( (("",(0,0)),["_","_",(0,0)]) ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((priority,[Ter token_name_LIDENT],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_LIDENT _1] -> Obj_priority ( _1 ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((priority,[],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [] -> Obj_priority ( ("default_priority",(-1,-1,-1)) ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((nested,[Ter token_name_LPAREN;Non_ter (rhs_list,No_priority );Ter token_name_RPAREN],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [ _1;Obj_rhs_list _2; _3] -> let ob,b,d =  let nt = "dypgen__nested_nt_"^(string_of_int _data) in
      let f (rl1,rl2) (prio,litl,pa_l,code,add_rules) =
        (nt,prio,litl,pa_l,code)::rl1,add_rules@rl2
      in
      let rl1,rl2 = List.fold_left f ([],[]) (List.rev _2) in
      ((nt,(0,0,0)),rl2@rl1),true,(_data+1)  in ((Obj_nested ob),b,d,_dd,_ld,[],[],_prd,None,None)
 | _ -> failwith "Invalid number or kind of arguments in action")))
;
((nested,[Ter token_name_LIDENT],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_LIDENT _1] -> Obj_nested ( (_1,[]) ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((literal,[Ter token_name_LIDENT;Ter token_name_LPAREN;Ter token_name_GREATER;Ter token_name_LIDENT;Ter token_name_RPAREN],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_LIDENT _1; _2; _3;Obj_LIDENT _4; _5] -> Obj_literal ( (Obj_non_terminal ((fst _1),_4,Pr_greater,1)),[] ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((literal,[Ter token_name_LIDENT;Ter token_name_LPAREN;Ter token_name_GREATER;Ter token_name_EQUAL;Ter token_name_LIDENT;Ter token_name_RPAREN],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_LIDENT _1; _2; _3; _4;Obj_LIDENT _5; _6] -> Obj_literal ( (Obj_non_terminal ((fst _1),_5,Pr_greatereq,1)),[] ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((literal,[Ter token_name_LIDENT;Ter token_name_LPAREN;Ter token_name_LESS;Ter token_name_LIDENT;Ter token_name_RPAREN],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_LIDENT _1; _2; _3;Obj_LIDENT _4; _5] -> Obj_literal ( (Obj_non_terminal ((fst _1),_4,Pr_less,1)),[] ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((literal,[Ter token_name_LIDENT;Ter token_name_LPAREN;Ter token_name_LESS;Ter token_name_EQUAL;Ter token_name_LIDENT;Ter token_name_RPAREN],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_LIDENT _1; _2; _3; _4;Obj_LIDENT _5; _6] -> Obj_literal ( (Obj_non_terminal ((fst _1),_5,Pr_lesseq,1)),[] ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((literal,[Ter token_name_LIDENT;Ter token_name_LPAREN;Ter token_name_EQUAL;Ter token_name_LIDENT;Ter token_name_RPAREN],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_LIDENT _1; _2; _3;Obj_LIDENT _4; _5] -> Obj_literal ( (Obj_non_terminal ((fst _1),_4,Pr_eq,1)),[] ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((literal,[Non_ter (nested,No_priority )],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_nested _1] -> Obj_literal (
      (Obj_non_terminal ((fst (fst _1)),
        ("No_priority",(-1,-1,-1)),Pr_eq,1)),
      (snd _1) ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((literal,[Ter token_name_UIDENT],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_UIDENT _1] -> Obj_literal ( (Obj_terminal _1),[] ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((literal_list,[Non_ter (literal_list,No_priority );Non_ter (opt_par_act,No_priority );Non_ter (literal,No_priority );Non_ter (opt_pattern,No_priority )],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_literal_list _1;Obj_opt_par_act _2;Obj_literal _3;Obj_opt_pattern _4] -> Obj_literal_list ( let l,len,part_act_l,add_rule1 = _1 in
        if len=0 && _2<>(("",(0,0)),[("_","_",(0,0))])
        then raise Giveup else
        let part_act_l =
          let pa,patl = _2 in
          if pa=("",(0,0)) then part_act_l
          else ((pa,len),patl)::part_act_l
        in
        let lit,add_rule2 = _3 in
        let pat_typ = match lit with
          | Obj_terminal s -> fst s
          | Obj_non_terminal (s,_,_,_) -> s
        in
        ( ((lit,([fst _4,pat_typ,snd _4]:((string*string*(int*int)) list)))::l), (len+1),
          part_act_l, add_rule2@add_rule1 ) ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((literal_list,[],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [] -> Obj_literal_list ( ([],0,[],[]) ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((opt_bar,[Ter token_name_BAR],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [ _1] -> Obj_opt_bar ( () ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((opt_bar,[],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [] -> Obj_opt_bar ( () ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((rhs,[Non_ter (literal_list,No_priority );Ter token_name_OCAML_CODE;Non_ter (priority,No_priority )],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_literal_list _1;Obj_OCAML_CODE _2;Obj_priority _3] -> Obj_rhs ( let litl,_,part_act_l,additional_rules = _1 in
        (_3,List.rev litl, part_act_l, _2, additional_rules) ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((rhs_list,[Non_ter (rhs_list,No_priority );Ter token_name_BAR;Non_ter (rhs,No_priority )],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_rhs_list _1; _2;Obj_rhs _3] -> Obj_rhs_list ( _3::_1 ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((rhs_list,[Non_ter (rhs,No_priority )],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_rhs _1] -> Obj_rhs_list ( [_1] ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((entry_def,[Ter token_name_LIDENT;Ter token_name_COLON;Non_ter (opt_bar,No_priority );Non_ter (rhs_list,No_priority )],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_LIDENT _1; _2;Obj_opt_bar _3;Obj_rhs_list _4] -> Obj_entry_def ( let f (rl1,rl2) (prio,litl,pa_l,code,add_rules) =
      (fst _1,prio,litl,pa_l,code)::rl1,add_rules@rl2
    in
    let rl1,rl2 = List.fold_left f ([],[]) (List.rev _4) in
    rl2@rl1 ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((grammar,[Non_ter (entry_def,No_priority )],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_entry_def _1] -> Obj_grammar ( _1 ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((grammar,[Non_ter (grammar,No_priority );Non_ter (entry_def,No_priority )],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_grammar _1;Obj_entry_def _2] -> Obj_grammar ( _2@_1 ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((relation_list,[Non_ter (relation_list,No_priority );Ter token_name_LESS;Ter token_name_LIDENT],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_relation_list _1; _2;Obj_LIDENT _3] -> Obj_relation_list ( (fst _3)::_1 ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((relation_list,[Ter token_name_LIDENT;Ter token_name_LESS;Ter token_name_LIDENT],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_LIDENT _1; _2;Obj_LIDENT _3] -> Obj_relation_list ( [(fst _3);(fst _1)] ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((relation,[Non_ter (relation,No_priority );Ter token_name_LIDENT],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_relation _1;Obj_LIDENT _2] -> Obj_relation ( (Rel_single (fst _2))::_1 ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((relation,[Non_ter (relation,No_priority );Non_ter (relation_list,No_priority )],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_relation _1;Obj_relation_list _2] -> Obj_relation ( (Rel_list (List.rev _2))::_1 ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((relation,[Ter token_name_KWD_RELATION],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [ _1] -> Obj_relation ( [] ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((token_list,[Non_ter (token_list,No_priority );Ter token_name_OCAML_TYPE;Ter token_name_UIDENT],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_token_list _1;Obj_OCAML_TYPE _2;Obj_UIDENT _3] -> Obj_token_list ( ((fst _3),_2)::_1 ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((token_list,[Non_ter (token_list,No_priority );Ter token_name_UIDENT],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_token_list _1;Obj_UIDENT _2] -> Obj_token_list ( ((fst _2),"No_type")::_1 ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((token_list,[Ter token_name_KWD_TOKEN;Ter token_name_OCAML_TYPE;Ter token_name_UIDENT],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [ _1;Obj_OCAML_TYPE _2;Obj_UIDENT _3] -> Obj_token_list ( [((fst _3),_2)] ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((token_list,[Ter token_name_KWD_TOKEN;Ter token_name_UIDENT],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [ _1;Obj_UIDENT _2] -> Obj_token_list ( [((fst _2),"No_type")] ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((optional_mli,[Ter token_name_KWD_MLI;Ter token_name_OCAML_CODE],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [ _1;Obj_OCAML_CODE _2] -> Obj_optional_mli ( _2 ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((optional_mli,[],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [] -> Obj_optional_mli ( ("",(0,0)) ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((optional_code,[Ter token_name_OCAML_CODE],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_OCAML_CODE _1] -> Obj_optional_code ( _1 ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((optional_code,[],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [] -> Obj_optional_code ( ("",(0,0)) ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((uident_list,[Non_ter (uident_list,No_priority );Ter token_name_UIDENT],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_uident_list _1;Obj_UIDENT _2] -> Obj_uident_list ( (fst _2)::_1 ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((uident_list,[Ter token_name_UIDENT],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_UIDENT _1] -> Obj_uident_list ( [(fst _1)] ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((lident_list,[Non_ter (lident_list,No_priority );Ter token_name_LIDENT],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_lident_list _1;Obj_LIDENT _2] -> Obj_lident_list ( (fst _2)::_1 ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((lident_list,[Ter token_name_LIDENT],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_LIDENT _1] -> Obj_lident_list ( [(fst _1)] ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((parser_param_infos,[Non_ter (parser_param_infos,No_priority );Non_ter (parser_param_info,No_priority )],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_parser_param_infos _1;Obj_parser_param_info _2] -> let ob,b,d = 
      { token_list = (_2.token_list@_1.token_list);
        relation = _2.relation@_1.relation;
        start = _2.start@_1.start;
        generic_merge = _2.generic_merge@_1.generic_merge;
        cons = _2.cons@_1.cons;
        additional_cons = _2.additional_cons@_1.additional_cons;
        nt_type = _2.nt_type@_1.nt_type;
        single_nt = _2.single_nt@_1.single_nt },
        false,_data  in ((Obj_parser_param_infos ob),b,d,_dd,_ld,[],[],_prd,None,None)
 | _ -> failwith "Invalid number or kind of arguments in action")))
;
((parser_param_infos,[Non_ter (parser_param_info,No_priority )],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_parser_param_info _1] -> Obj_parser_param_infos ( _1 ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((parser_param_info,[Ter token_name_KWD_NON_TERMINAL;Non_ter (lident_list,No_priority )],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [ _1;Obj_lident_list _2] -> Obj_parser_param_info ( {empty_ppi with single_nt = List.rev _2} ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((parser_param_info,[Ter token_name_KWD_TYPE;Ter token_name_OCAML_TYPE;Non_ter (lident_list,No_priority )],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [ _1;Obj_OCAML_TYPE _2;Obj_lident_list _3] -> Obj_parser_param_info ( {empty_ppi with nt_type = [_2,List.rev _3]} ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((parser_param_info,[Ter token_name_KWD_MERGE;Ter token_name_LIDENT;Non_ter (lident_list,No_priority )],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [ _1;Obj_LIDENT _2;Obj_lident_list _3] -> Obj_parser_param_info ( {empty_ppi with generic_merge = [((fst _2),List.rev _3)]} ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((parser_param_info,[Ter token_name_KWD_CONSTRUCTOR;Non_ter (uident_list,No_priority )],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [ _1;Obj_uident_list _2] -> Obj_parser_param_info ( { empty_ppi with additional_cons = List.rev _2 }  ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((parser_param_info,[Ter token_name_KWD_CONSTRUCTOR;Ter token_name_UIDENT;Ter token_name_KWD_FOR;Non_ter (lident_list,No_priority )],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [ _1;Obj_UIDENT _2; _3;Obj_lident_list _4] -> Obj_parser_param_info ( { empty_ppi with cons = [(fst _2,List.rev _4)] }  ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((parser_param_info,[Non_ter (relation,No_priority )],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_relation _1] -> Obj_parser_param_info ( {empty_ppi with relation = _1} ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((parser_param_info,[Non_ter (token_list,No_priority )],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_token_list _1] -> Obj_parser_param_info ( {empty_ppi with token_list = _1} ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((parser_param_info,[Ter token_name_KWD_START;Ter token_name_OCAML_TYPE;Ter token_name_LIDENT],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [ _1;Obj_OCAML_TYPE _2;Obj_LIDENT _3] -> Obj_parser_param_info ( {empty_ppi with start = [((fst _3),_2)]} ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((optional_trailer,[Ter token_name_PERCENTPERCENT;Ter token_name_OCAML_CODE],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [ _1;Obj_OCAML_CODE _2] -> Obj_optional_trailer ( _2 ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((optional_trailer,[],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [] -> Obj_optional_trailer ( ("",(0,0)) ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))
;
((main,[Non_ter (optional_code,No_priority );Non_ter (parser_param_infos,No_priority );Ter token_name_PERCENTPERCENT;Non_ter (grammar,No_priority );Non_ter (optional_trailer,No_priority );Non_ter (optional_mli,No_priority );Ter token_name_EOF],default_priority),Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _prd _ -> (match action_variable__l with [Obj_optional_code _1;Obj_parser_param_infos _2; _3;Obj_grammar _4;Obj_optional_trailer _5;Obj_optional_mli _6; _7] -> Obj_main ( (_1,_2,List.rev _4,_5,_6) ) | _ -> failwith "Invalid number of arguments in action"),true,_data,_dd,_ld,[],[],_prd,None,None))]
let current_priority_data = empty_priority_data

let merge_entry_def ol _ = ol
let merge_grammar ol _ = ol
let merge_lident_list ol _ = ol
let merge_literal ol _ = ol
let merge_literal_list ol _ = ol
let merge_main ol _ = ol
let merge_nested ol _ = ol
let merge_opt_bar ol _ = ol
let merge_opt_par_act ol _ = ol
let merge_opt_pattern ol _ = ol
let merge_optional_code ol _ = ol
let merge_optional_mli ol _ = ol
let merge_optional_trailer ol _ = ol
let merge_parser_param_info ol _ = ol
let merge_parser_param_infos ol _ = ol
let merge_priority ol _ = ol
let merge_relation ol _ = ol
let merge_relation_list ol _ = ol
let merge_rhs ol _ = ol
let merge_rhs_list ol _ = ol
let merge_token_list ol _ = ol
let merge_uident_list ol _ = ol

let merge_map = P.Tools.init_merge_map [merge_uident_list,22;merge_token_list,21;merge_rhs_list,20;merge_rhs,19;merge_relation_list,18;merge_relation,17;merge_priority,16;merge_parser_param_infos,15;merge_parser_param_info,14;merge_optional_trailer,13;merge_optional_mli,12;merge_optional_code,11;merge_opt_pattern,10;merge_opt_par_act,9;merge_opt_bar,8;merge_nested,7;merge_main,6;merge_literal_list,5;merge_literal,4;merge_lident_list,3;merge_grammar,2;merge_entry_def,1]let parsing_device = create_parsing_device rapf_list empty_priority_data `LR0 0 0 merge_map merge P.Tools.empty_datadyn nt_names (Array.make (Array.length nt_names) 0)
let main f lexbuf =
  let data_equal = {
    P.Tools.global_data_equal = (==);
    P.Tools.local_data_equal = (==) }
  in
  let lexbuf_position lexbuf = (lexbuf.Lexing.lex_curr_p,lexbuf.Lexing.lex_start_p) in
  let pf = glrParse parsing_device __dypgen_get_value get_name str_token main data_equal  [|fun _ -> true|] (fun _ -> "") f lexbuf lexbuf_position in
  let aux1 (o,p) = match o with
    | Obj_main r -> (r,p) | _ -> failwith "Wrong type for entry result" in
  List.map aux1 pf

