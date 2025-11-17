open OUnit2
open FinalProject
open ImageOps

let white = Graphics.rgb 255 255 255

let tests =
  "FinalProject Tests"
  >::: [
         ( "Test cut_square" >:: fun _ ->
           let result =
             cut_square [| [| 0; 12 |]; [| 42; 3110 |] |] (0, 0) (1, 1)
           in
           let expected = [| [| white; white |]; [| white; white |] |] in
           assert_equal expected result;
           let result =
             cut_square
               [| [| 0; 12; 34 |]; [| 42; 3110; 56 |]; [| 78; 90; 100 |] |]
               (1, 1) (2, 2)
           in
           let expected =
             [|
               [| 0; 12; 34 |]; [| 42; white; white |]; [| 78; white; white |];
             |]
           in
           assert_equal expected result;
           let result = cut_square [| [| 1 |] |] (0, 0) (0, 0) in
           let expected = [| [| white |] |] in
           assert_equal expected result;
           let result =
             cut_square (Array.make_matrix 1000 1000 0) (500, 500) (600, 600)
           in
           let expected =
             Array.init 1000 (fun y ->
                 Array.init 1000 (fun x ->
                     if 500 <= x && x <= 600 && 500 <= y && y <= 600 then white
                     else 0))
           in
           assert_equal expected result );
         ( "Test intersects_segment" >:: fun _ ->
           assert_equal true (intersects_segment (5, 5) (0, 0) (10, 10));
           assert_equal true (intersects_segment (0, 5) (0, 10) (10, 0));
           assert_equal true (intersects_segment (10, 0) (0, 10) (10, 0));
           assert_equal true (intersects_segment (0, 10) (0, 10) (10, 0));
           assert_equal false (intersects_segment (15, 15) (0, 0) (10, 10));
           assert_equal false (intersects_segment (5, 15) (0, 0) (10, 10));
           assert_equal false (intersects_segment (10, 5) (0, 0) (10, 10)) );
       ]

let _ = run_test_tt_main tests
