
open Cfg
open Cfg.Abbr
open Visitor
open Utils
open Cfg_printer.CodeUnparser

module CR = Cfg_refactor
module Pr = Cfg_printer.CodePrinter
module C = Cfg.Abbr

let symbol_of_msg_id = function
  | `ID_UOperator uop -> 
      `Lit_Atom (format_to_string Pr.format_unary_op uop)
  | `ID_Operator bop ->
      `Lit_Atom (format_to_string Pr.format_binary_op bop)
  | `ID_MethodName s -> `Lit_Atom s
  | `ID_Assign s -> `Lit_Atom (s ^ "=")
  | `ID_Super -> `Lit_Atom "super"

let call_wrap code e pos = 
  let tmp = C.local "__druby_safe_eval_temp" in
  let targ = C.access_path ["DRuby";"Contract";"Wrap"] in
  let msg = `ID_MethodName "wrap" in
  let args = [C.str pos.Lexing.pos_fname;
              C.num pos.Lexing.pos_lnum;
              C.str code;
              e]
  in
    tmp, C.mcall ~lhs:tmp ~targ msg args pos

let contract_call code lhs_o mc pos = 
  let init_recv = default_opt `ID_Self mc.mc_target in
  let tmp, wrap_stmt = call_wrap code init_recv pos in
  let args = symbol_of_msg_id mc.mc_msg::mc.mc_args in
  let msg = `ID_MethodName "send" in
  let mc' = {mc_target=Some tmp;mc_msg=msg;mc_args=args;mc_cb=mc.mc_cb} in
    C.seq [wrap_stmt;mkstmt (MethodCall(lhs_o,mc')) pos] pos

(** Wrap a tuple and return a ChangeTo action *)
let change_tup code f tup pos = 
  let tmp = C.local "__druby_safe_eval_temp_tuple" in
  let tmp_asgn = C.assign tmp tup pos in
  let tmp2, call = call_wrap code tmp pos in
  let stmt = C.seq [tmp_asgn; call;mkstmt (f tmp2) pos] pos in
    ChangeTo stmt

let blacklist = [
  "remove_method";
  "undef_method";
  "append_features";
]

let failure msg fmt = 
  let code = 
    s"STDERR.puts %{" ++ string ++ s"};" ++
    s"STDERR.puts %{  The error was: " ++ fmt ++ s"}; "++
    s"exit(1)" 
  in
    CR.kreparse (fun x -> ChangeTo x) code msg

class rewrite_visitor (code:string) fail_msg = 
object(self)
  inherit default_visitor as super

  method visit_stmt stmt = match stmt.snode with
    | Assign(`ID_Var(`Var_Local,_),_) -> DoChildren

    | Assign(left,(#expr as rhs)) ->
        let tmp, wrap = call_wrap code rhs stmt.pos in
          ChangeTo(C.seq [wrap;C.assign left tmp stmt.pos] stmt.pos)

    | Assign(left,rhs) -> change_tup code (fun x -> Assign(left,x)) rhs stmt.pos

    | MethodCall(_,{mc_msg=`ID_MethodName str}) when List.mem str blacklist ->
        failure fail_msg (s"blacklisted method: " ++ string) str

    | MethodCall(lhs, mc) ->  ChangeTo (contract_call code lhs mc stmt.pos)

    | Method(dn,formals,body) ->
        failure fail_msg (s"new method definition: " ++ def_name) dn

    | Undef lst -> failure fail_msg (s"blacklisted construct: undef")

    | Return(Some tup) -> change_tup code (fun x -> Return (Some x)) tup stmt.pos
    | Break(Some tup) -> change_tup code (fun x -> Break (Some x)) tup stmt.pos
    | Next(Some tup) -> change_tup code (fun x -> Next (Some x)) tup stmt.pos
        
    (* yield : should be captured by the Ruby instrumentation *)
    (* TODO: wrap constant definitions already defined in subclass *)
    (* Module/Class : same problem? *)

    | _ -> DoChildren
end

let read_stdin () = 
  let buf = Buffer.create 128 in
  let str = String.create 128 in
    try while true do
      let num_read = input stdin str 0 128 in
        if num_read > 0 
        then Buffer.add_substring buf str 0 num_read
        else raise End_of_file
    done; assert false
    with End_of_file -> Buffer.contents buf

open Yaml
module Input = Y4Tuple(YString)(YInt)(YString)(YList(YString))

let violation_msg filename lineno code = 
  let fmt = 
    "[ERROR]\n" ^^
      "  The DRuby runtime system detected an error in a previuosly unseen call to eval\n" ^^
      "  The code, located at %s:%d\n" ^^
      "  was the string [%s]\n" 
  in
    Printf.sprintf fmt filename lineno code

let () = 
  let str = read_stdin() in
  let filename,lineno,code,locals = Input.of_yaml (Yaml.parse_string str) in
  let env = List.fold_left (fun acc x -> StrSet.add x acc) StrSet.empty locals in
  let ast = Parse_helper.parse_string ~env ~filename ~lineno code in
  let cfg = CR.refactor_ast ~env ast in
  let fail_msg = violation_msg filename lineno code in
  let cfg = visit_stmt (new rewrite_visitor code fail_msg) cfg in
    Cfg_printer.CodePrinter.print_stmt stdout cfg
