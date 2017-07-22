
open OUnit
open Cfg_test_helper
open Config


let succ_scope_test (desc,pre,post) = 
  conf.error_raises_exc <- true;
  let d = desc ^ ": "  ^ pre in
    d >::
      (fun () -> 
	 Cfg_refactor.re_init ();
	 let loader = File_loader.create File_loader.EmptyCfg [] in
         let scoper = Cfg_scope.create loader in
	 let cfg1 = Cfg_scope.resolve_scopes scoper (refactor_string pre) in
	 let cfg2 = Cfg_scope.resolve_scopes scoper (refactor_string post) in
	   assert_cfg_equal cfg1 cfg2
      )

let diff_scope_test (desc,pre,post) = 
  conf.error_raises_exc <- true;
  let d = desc ^ ": "  ^ pre in
    d >::
      (fun () -> 
	 Cfg_refactor.re_init ();
	 let loader = File_loader.create File_loader.EmptyCfg [] in
         let scoper = Cfg_scope.create loader in
	 let cfg1 = Cfg_scope.resolve_scopes scoper (refactor_string pre) in
	 let cfg2 = Cfg_scope.resolve_scopes scoper (refactor_string post) in
	   assert_cfg_neq cfg1 cfg2
      )

exception Should_fail
let fail_scope_test (desc,code) = 
  conf.error_raises_exc <- true;
  let d = desc ^ ": "  ^ code in
    d >::
      (fun () -> 
	 try 
	 let loader = File_loader.create File_loader.EmptyCfg [] in
         let scoper = Cfg_scope.create loader in
	     ignore(Cfg_scope.resolve_scopes scoper (refactor_string code));
             raise Should_fail
	 with Failure _ -> ()
      )


let succ_tests = [
  ("simple uscope",
   "A=1;A",
   "A=1;::A"
  );

  ("nested access to uscope",
   "A=1;class B;A;end",
   "A=1;class B;::A;end"
  );

  ("nested access to scope",
   "class A; class B; class C;
    A::B; end end end",
   "class A; class B; class C;
    ::A::B; end end end"
  );

  ("subtrees with same scope id",
"class A
  class B
    class C
      C
    end
  end
  class D
    class C;
      C
    end
   end
end",
"class A
  class B
    class C
      ::A::B::C
    end
  end
  class D
    class C;
      ::A::D::C
    end
   end
end"
  );

  ("inherit constant over lexical scope",
"class Module; X=2;end
class A
  class Module; X=1; end
  class B < Module;  X; end
end
",
"class ::Module; ::Module::X=2;end
class ::A
  class Module; ::A::Module::X=1; end
  class ::A::B < ::A::Module; ::A::Module::X; end
end
");

  ("reference superclass constant",
   "class A; X=1;end;
    class B < A; X; end",
   "class A; X=1;end;
    class B < A; ::A::X; end"
  );

  ("scoped initializer",
   "class A;def m(); X; end;end
    A::X = 1",
   "class A;def m(); ::A::X; end;end
    ::A::X = 1"
  );

  ("module include",
   "module M; X=1;end
    class A; include M; X;end",
   "module ::M; ::M::X=1;end
    class ::A; include ::M; ::M::X;end"
  );

  ("re-enter scope",
   "class A; class B;  X=1; end;end
    class ::A::B; X;end",
   "class ::A; class ::A::B; ::A::B::X=1; end;end
    class ::A::B; ::A::B::X;end"
  );

  ("class method lex scope",
   "class A; X=1
    def A.foo; X; end;end",
   "class ::A; ::A::X=1
    def ::A.foo; ::A::X; end;end"
  );

  ("singleton method doesn't open lexical scope",
"X=1
class Outer
  X=2
  class A
    class << self
      class X
      end
      def testme1
        X
      end
    end
    def self.testme2
      X
    end
  end
end
",
"X=1
class Outer
  X=2
  class A
    class << self
      class X
      end
      def testme1
        ::Outer::A::X
      end
    end
    def self.testme2
      ::Outer::X
    end
  end
end
");

  (* just want this to be accepted, I don't think we want to change
     the self to a ::A *)
  ("inherit from self",
   "class A; class B < self; end;end",
   "class A; class B < self; end;end"
  );

  ("track classes through assignment",
"class A; X=1; end;B=A;B::X",
"class ::A; ::A::X=1; end;::B=::A;::A::X"
  );


  ("equal named parent/child",
   "module A; class A; end; end",
   "module ::A; class ::A::A; end; end"
  );

  ("equal named parent/child, look in parent",
"module A
  class A
  end

  class B
    def B.foo() end
  end
end",
"module ::A
  class ::A::A
  end

  class ::A::B
    def ::A::B.foo() end
  end
end");


(*
  ("metaclass looks in reg class",
  "
  class A
  X=1
  class B
  X=2
  end
  end
  X=3
  class << A::B.new
  puts X
  end
  ","
  class A
  X=1
  class B
  X=2
  end
  end
  X=3
  class << A::B.new
  puts ::A::B::X
  end
  "  
  );
*)
]

let diff_tests = [
  ("don't break lexical scope",
   "class A; class B; end; end",
   "class A;end;class ::A::B;end"
  );

]

let fail_tests = [
  ("lexical binder",
   "class A; X=1;end
    class ::A::B; X; end"
  )

]

let (@@) = List.rev_append

let suite = "Scope suite" >:::
  (List.map succ_scope_test succ_tests) @@
  (List.map diff_scope_test diff_tests) @@
  (List.map fail_scope_test fail_tests)
  
