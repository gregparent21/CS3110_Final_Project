open OUnit2
open FinalProject
open ImageOps

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
         ( "Test invert_colors twice is identity" >:: fun _ ->
           let p1 = Graphics.rgb 10 20 30 in
           let p2 = Graphics.rgb 200 150 100 in
           let img = [| [| p1; p2 |]; [| p2; p1 |] |] in
           let result = invert_colors (invert_colors img) in
           assert_equal img result );
         ( "Test crop bottom-left 2x2" >:: fun _ ->
           let img =
             [| [| 10; 11; 12 |]; [| 20; 21; 22 |]; [| 30; 31; 32 |] |]
           in
           let result = crop img (0, 0) (1, 1) in
           let expected = [| [| 20; 21 |]; [| 30; 31 |] |] in
           assert_equal expected result );
         ( "Test crop reversed coords" >:: fun _ ->
           let img =
             [| [| 10; 11; 12 |]; [| 20; 21; 22 |]; [| 30; 31; 32 |] |]
           in
           let result = crop img (1, 1) (0, 0) in
           let expected = [| [| 20; 21 |]; [| 30; 31 |] |] in
           assert_equal expected result );
         ( "Test crop single pixel" >:: fun _ ->
           let img =
             [| [| 10; 11; 12 |]; [| 20; 21; 22 |]; [| 30; 31; 32 |] |]
           in
           let result = crop img (2, 1) (2, 1) in
           let expected = [| [| 22 |] |] in
           assert_equal expected result );
       ]

let _ = run_test_tt_main tests
