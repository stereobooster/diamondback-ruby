
open OUnit
open Config

module ProfiledProgs = struct
  let dir = "profile"
  let success_dir = "profile/succeed"
  let fail_dir = "profile/fail"
  let old_profile = ref false
  let setup _ = 
    old_profile := conf.profile;
    conf.profile <- true

  let teardown _ = 
    conf.profile <- !old_profile
end

module ProfileTests = Prog_test.Make(ProfiledProgs)

let suite = "Profile Suite" >:::
  [ProfileTests.suite;
  ]

