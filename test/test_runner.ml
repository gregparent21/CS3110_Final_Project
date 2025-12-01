open OUnit2

let () =
  let suite =
    "All Tests" >::: [ Test_FinalProject.tests; Test_Image_Loading.tests ]
  in
  run_test_tt_main suite
