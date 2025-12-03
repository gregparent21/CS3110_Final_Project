open OUnit2
open FinalProject
open ImageOps

let white = Graphics.rgb 255 255 255

(*Helper function used to make a grey-scaled pixel*)
let c v = Graphics.rgb v v v

let tests =
  "ImageOps Tests"
  >::: [
         ( "Test flip_horizontal" >:: fun _ ->
           let img = [| [| 1; 2; 3 |]; [| 4; 5; 6 |] |] in
           let result = flip_horizontal img in
           let expected = [| [| 3; 2; 1 |]; [| 6; 5; 4 |] |] in
           assert_equal expected result );
         ( "Test shrink 2x2 -> 1x1" >:: fun _ ->
           let a = Graphics.rgb 10 20 30 in
           let b = Graphics.rgb 20 30 40 in
           let c = Graphics.rgb 30 40 50 in
           let d = Graphics.rgb 40 50 60 in
           let img = [| [| a; b |]; [| c; d |] |] in
           let result = shrink img in
           let expected = [| [| Graphics.rgb 25 35 45 |] |] in
           assert_equal expected result );
         ( "Test shrink 4x4 -> 2x2" >:: fun _ ->
           let img =
             [|
               [| c 10; c 20; c 70; c 80 |];
               [| c 30; c 40; c 90; c 100 |];
               [| c 10; c 20; c 70; c 80 |];
               [| c 30; c 40; c 90; c 100 |];
             |]
           in
           let result = shrink img in
           let expected = [| [| c 25; c 85 |]; [| c 25; c 85 |] |] in
           assert_equal expected result );
         ( "Test pixelate (factor 2)" >:: fun _ ->
           let v r = Graphics.rgb r r r in
           let img =
             [|
               [| v 10; v 20; v 50; v 60 |];
               [| v 30; v 40; v 70; v 80 |];
               [| v 10; v 20; v 50; v 60 |];
               [| v 30; v 40; v 70; v 80 |];
             |]
           in
           let _ = pixelate img 2 in
           let block1 = v 25 in
           let block2 = v 65 in
           let expected =
             [|
               [| block1; block1; block2; block2 |];
               [| block1; block1; block2; block2 |];
               [| block1; block1; block2; block2 |];
               [| block1; block1; block2; block2 |];
             |]
           in
           assert_equal expected img );
         ( "Test mirror (alias flip_horizontal)" >:: fun _ ->
           let img = [| [| 10; 20 |]; [| 30; 40 |] |] in
           let result = flip_horizontal img in
           let expected = [| [| 20; 10 |]; [| 40; 30 |] |] in
           assert_equal expected result );
         ( "Test invert_colors" >:: fun _ ->
           let p1 = Graphics.rgb 10 20 30 in
           let p2 = Graphics.rgb 0 255 128 in
           let img = [| [| p1; p2 |] |] in
           let result = invert_colors img in
           let expected =
             [|
               [|
                 Graphics.rgb (255 - 10) (255 - 20) (255 - 30);
                 Graphics.rgb (255 - 0) (255 - 255) (255 - 128);
               |];
             |]
           in
           assert_equal expected result );
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
               [| 0; white; white |]; [| 42; white; white |]; [| 78; 90; 100 |];
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
             Array.init 1000 (fun temp ->
                 let y = 1000 - temp - 1 in
                 Array.init 1000 (fun x ->
                     if 500 <= x && x <= 600 && 500 <= y && y <= 600 then white
                     else 0))
           in
           assert_equal expected result );
         ( "Test cut" >:: fun _ ->
           let polygon = [ (1, 1); (4, 1); (4, 4); (1, 4) ] in
           let result =
             cut
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
           let result = cut [| [| 1 |] |] [ (0, 0); (0, 0); (0, 0) ] in
           let expected = [| [| white |] |] in
           assert_equal expected result;

           let result =
             cut
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
             cut
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
             cut (Array.make_matrix 5 5 0) [ (0, 0); (2, 4); (4, 0) ]
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
             cut (Array.make_matrix 7 7 0)
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
         ( "Test fill_square" >:: fun _ ->
           let result =
             fill_square [| [| 0; 12 |]; [| 42; 3110 |] |] (0, 0) (1, 1) 99
           in
           let expected = [| [| 99; 99 |]; [| 99; 99 |] |] in
           assert_equal expected result;
           let result =
             fill_square
               [| [| 0; 12; 34 |]; [| 42; 3110; 56 |]; [| 78; 90; 100 |] |]
               (1, 1) (2, 2) 100
           in
           let expected =
             [| [| 0; 100; 100 |]; [| 42; 100; 100 |]; [| 78; 90; 100 |] |]
           in
           assert_equal expected result );
         ( "Test fill" >:: fun _ ->
           let polygon = [ (1, 1); (4, 1); (4, 4); (1, 4) ] in
           let result =
             fill
               [|
                 [| 0; 0; 0; 0; 0 |];
                 [| 0; 12; 34; 56; 0 |];
                 [| 0; 78; 90; 100; 0 |];
                 [| 0; 110; 120; 130; 0 |];
                 [| 0; 0; 0; 0; 0 |];
               |]
               polygon 100
           in
           let expected =
             [|
               [| 0; 100; 100; 100; 100 |];
               [| 0; 100; 100; 100; 100 |];
               [| 0; 100; 100; 100; 100 |];
               [| 0; 100; 100; 100; 100 |];
               [| 0; 0; 0; 0; 0 |];
             |]
           in
           assert_equal expected result;
           let result =
             fill (Array.make_matrix 5 5 0) [ (0, 0); (2, 4); (4, 0) ] 100
           in
           let expected =
             [|
               [| 0; 0; 100; 0; 0 |];
               [| 0; 0; 100; 0; 0 |];
               [| 0; 100; 100; 100; 0 |];
               [| 0; 100; 100; 100; 0 |];
               [| 100; 100; 100; 100; 100 |];
             |]
           in
           assert_equal expected result );
       ]

let _ = run_test_tt_main tests
