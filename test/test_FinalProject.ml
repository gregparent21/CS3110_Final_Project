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
           assert_equal true (intersects_segment (0, 10) (0, 10) (10, 0));
           assert_equal false (intersects_segment (15, 15) (0, 0) (10, 10));
           assert_equal false (intersects_segment (5, 15) (0, 0) (10, 10));
           assert_equal false (intersects_segment (10, 5) (0, 0) (10, 10)) );
         ( "Test cut_advanced" >:: fun _ ->
           let polygon = [ (1, 1); (4, 1); (4, 4); (1, 4) ] in
           let result =
             cut_advanced
               [|
                 [| 0; 0; 0; 0; 0 |];
                 [| 0; 12; 34; 56; 0 |];
                 [| 0; 78; 90; 100; 0 |];
                 [| 0; 110; 120; 130; 0 |];
                 [| 0; 0; 0; 0; 0 |];
               |]
               polygon
           in
           let expected =
             [|
               [| 0; white; white; white; white |];
               [| 0; white; white; white; white |];
               [| 0; white; white; white; white |];
               [| 0; white; white; white; white |];
               [| 0; 0; 0; 0; 0 |];
             |]
           in
           assert_equal expected result;
           let result = cut_advanced [| [| 1 |] |] [ (0, 0); (0, 0); (0, 0) ] in
           let expected = [| [| white |] |] in
           assert_equal expected result;

           let result =
             cut_advanced
               [|
                 [| 0; 0; 0; 0; 0; 0 |];
                 [| 0; 12; 34; 56; 78; 0 |];
                 [| 0; 90; 100; 110; 120; 0 |];
                 [| 0; 130; 140; 150; 160; 0 |];
                 [| 0; 170; 180; 190; 200; 0 |];
                 [| 0; 0; 0; 0; 0; 0 |];
               |]
               [ (1, 1); (4, 1); (4, 4); (1, 4) ]
           in
           let expected =
             [|
               [| 0; 0; 0; 0; 0; 0 |];
               [| 0; white; white; white; white; 0 |];
               [| 0; white; white; white; white; 0 |];
               [| 0; white; white; white; white; 0 |];
               [| 0; white; white; white; white; 0 |];
               [| 0; 0; 0; 0; 0; 0 |];
             |]
           in
           assert_equal expected result;
           let result =
             cut_advanced
               (Array.make_matrix 100 100 0)
               [ (20, 20); (80, 20); (80, 80); (20, 80) ]
           in
           let expected =
             Array.init 100 (fun y ->
                 Array.init 100 (fun x ->
                     if 20 <= x && x <= 80 && 19 <= y && y <= 79 then white
                     else 0))
           in
           assert_equal expected result;
           let result =
             cut_advanced (Array.make_matrix 5 5 0) [ (0, 0); (2, 4); (4, 0) ]
           in
           let expected =
             [|
               [| 0; 0; white; 0; 0 |];
               [| 0; 0; white; 0; 0 |];
               [| 0; white; white; white; 0 |];
               [| 0; white; white; white; 0 |];
               [| white; white; white; white; white |];
             |]
           in
           assert_equal expected result;
           let result =
             cut_advanced (Array.make_matrix 7 7 0)
               [ (2, 0); (4, 0); (6, 2); (3, 5); (0, 2) ]
           in
           let expected =
             [|
               [| 0; 0; 0; 0; 0; 0; 0 |];
               [| 0; 0; 0; white; 0; 0; 0 |];
               [| 0; 0; white; white; white; 0; 0 |];
               [| 0; white; white; white; white; white; 0 |];
               [| white; white; white; white; white; white; white |];
               [| 0; white; white; white; white; white; 0 |];
               [| 0; 0; white; white; white; 0; 0 |];
             |]
           in
           assert_equal expected result );
       ]

let _ = run_test_tt_main tests
