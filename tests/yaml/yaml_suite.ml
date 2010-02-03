
open OUnit
open Yaml

let yml_to_from_string yml = 
  let s = Yaml.emit_yaml_string yml in
    (*Printf.eprintf "emit: '%s'\n" s;*)
    Yaml.parse_string(s)

let yaml_to_from_string to_y from_y t = 
  from_y (yml_to_from_string (to_y t))

module Test(Y : YType)(X : sig val x : Y.t end) = struct
  let test desc = 
    let f () = 
      let x' = yaml_to_from_string Y.to_yaml Y.of_yaml X.x in
      let cmp x y = Y.compare x y = 0 in
      let printer x = yaml_to_string (Y.to_yaml x) in
        assert_equal ~cmp ~printer X.x x'
    in desc >:: f
end

let tests = [

  (let module T = Test(YUnit)
     (struct let x = () end) 
   in T.test "unit"
  );

  (let module T = Test(YInt)
     (struct let x = 3 end) 
   in T.test "int"
  );

  (let module T = Test(YInt)
     (struct let x = -3 end) 
   in T.test "neg int"
  );

  (let module T = Test(YBig_int)
     (struct let x = Big_int.big_int_of_string "9999999999999999999" end) 
   in T.test "big_int"
  );

  (let module T = Test(YBig_int)
     (struct let x = Big_int.big_int_of_string "-9999999999999999999" end) 
   in T.test "neg big_int"
  );

  (let module T = Test(YFloat)
     (struct let x = 3.0 end) 
   in T.test "float"
  );

  (let module T = Test(YFloat)
     (struct let x = -3.0 end) 
   in T.test "neg float"
  );

  (let module T = Test(YFloat)
     (struct let x = infinity end) 
   in T.test "inf float"
  );

  (let module T = Test(YFloat)
     (struct let x = neg_infinity end) 
   in T.test "neginf float"
  );

  (let module T = Test(YFloat)
     (struct let x = nan end) 
   in T.test "nan float"
  );

  (let module T = Test(YString)
     (struct let x = "hi" end) 
   in T.test "string"
  );

  (let module T = Test(YString)
     (struct let x = "" end) 
   in T.test "empty string"
  );

  (let module T = Test(YString)
     (struct let x = "\"hi'" end) 
   in T.test "quotes in string"
  );

  (let module T = Test(YBool)
     (struct let x = true end) 
   in T.test "bool/true"
  );

  (let module T = Test(YBool)
     (struct let x = false end) 
   in T.test "bool/false"
  );

  (let module T = Test(YOption(YInt))
     (struct let x = None end) 
   in T.test "option none"
  );

  (let module T = Test(YOption(YInt))
     (struct let x = Some 3 end) 
   in T.test "option some"
  );

  (let module T = Test(YPair(YInt)(YBool))
     (struct let x = 3,true end) 
   in T.test "pair"
  );

  (let module T = Test(Y3Tuple(YInt)(YBool)(YString))
     (struct let x = 3,true,"hi" end) 
   in T.test "3tuple"
  );

  (let module T = Test(Y4Tuple(YInt)(YBool)(YString)(YInt))
     (struct let x = 3,true,"hi",4 end) 
   in T.test "4tuple"
  );

  (let module T = Test(Y5Tuple(YInt)(YBool)(YString)(YInt)(YBool))
     (struct let x = 3,true,"hi",4,false end) 
   in T.test "5tuple"
  );

  (let module T = Test(YList(YInt))
     (struct let x = [1;2;3;4] end) 
   in T.test "list"
  );

  (let module T = Test(YList(YInt))
     (struct let x = [] end) 
   in T.test "empty list"
  );

  (let module T = Test(YAssocList(YInt)(YBool))
     (struct let x = [1,true;2,false;3,true;4,false] end) 
   in T.test "assoc list"
  );

  (let module T = Test(YAssocList(YInt)(YBool))
     (struct let x = [] end) 
   in T.test "empty assoc list"
  );

  (let module M = YSet(YInt) in
   let module T = Test(M)
     (struct let x = M.Set.add 4 (M.Set.singleton 3) end) 
   in T.test "set"
  );

  (let module M = YSet(YInt) in
   let module T = Test(M)
     (struct let x = M.Set.singleton 3 end) 
   in T.test "singleton set"
  );

  (let module M = YSet(YInt) in
   let module T = Test(M)
     (struct let x = M.Set.empty end) 
   in T.test "empty set"
  );

  (let module M = YMap(YInt)(YString) in
   let module T = Test(M)
     (struct let x = M.Map.add 4 "hi" (M.Map.add 3 "" M.Map.empty) end) 
   in T.test "map"
  );

  (let module M = YMap(YInt)(YString) in
   let module T = Test(M)
     (struct let x = M.Map.add 3 "" M.Map.empty end) 
   in T.test "singleton map"
  );

  (let module M = YMap(YInt)(YString) in
   let module T = Test(M)
     (struct let x = M.Map.empty end) 
   in T.test "empty map"
  );

  (let module M = YHashtbl(YInt)(YString) in
   let module T = Test(M)
     (struct 
        let x = Hashtbl.create 8
        let () = 
          Hashtbl.add x 4 "hi";
          Hashtbl.add x 3 ""
      end) 
   in T.test "hashtbl"
  );

  (let module M = YHashtbl(YInt)(YString) in
   let module T = Test(M)
     (struct 
        let x = Hashtbl.create 8
      end) 
   in T.test "empty hashtbl"
  );

  (let module M = YOr(YInt)(YString) in
   let module T = Test(M)
     (struct let x = `A 3 end) 
   in T.test "fst Or"
  );

  (let module M = YOr(YInt)(YString) in
   let module T = Test(M)
     (struct let x = `B "hi" end) 
   in T.test "snd Or"
  );

]

let suite = "YAML Suite" >:::
  tests
