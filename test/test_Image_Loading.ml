open OUnit2
open FinalProject
open ImageOps

let white = Graphics.rgb 255 255 255

let tests =
  "FinalProject Tests"
  >::: [
         ( "Test " >:: fun _ ->
           failwith "Havent Implemented");
       ]

let _ = run_test_tt_main tests
