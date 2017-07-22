
type 'a var = {
  vid : int;
  vctx : Log.ctx;
  vkind : 'a;
} 

type 'a t = 'a var Unify.t

let deref v = (Unify.value v).vkind
    
let vid t = (Unify.value t).vid

let vctx t = (Unify.value t).vctx

let compare t1 t2 = Pervasives.compare (vid t1) (vid t2)

let union v1 v2 = 
  Log.note "union %d => %d" (vid v1) (vid v2);
  ignore(Unify.union v1 v2)

let typ_var_count = ref 0

let create kind ~ctx = 
  incr typ_var_count;
  Unify.create {vid = !typ_var_count; 
     vctx = ctx; 
     vkind = kind}

let same t1 t2 = (Unify.ecr t1) == (Unify.ecr t2)

let format_var fmt_v ppf v = 
  Format.fprintf ppf "[%d]%a" (vid v) fmt_v (deref v)

let status s = 
  Printf.eprintf "Variable: %s, created %d vars\n" s !typ_var_count

(*
let () = 
  at_exit (fun () -> status "final")
*)
