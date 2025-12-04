open OUnit2
open FinalProject
open ImageOps

let white = Graphics.rgb 255 255 255

(*Helper function used to make a grey-scaled pixel*)
let c v = Graphics.rgb v v v
let r p = (p lsr 16) land 0xFF
let g p = (p lsr 8) land 0xFF
let b p = p land 0xFF

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
         ( "Test pixelate with factor 1 shouldn't change image" >:: fun _ ->
           let img =
             [|
               [| c 10; c 20; c 50; c 60 |];
               [| c 30; c 40; c 70; c 80 |];
               [| c 10; c 20; c 50; c 60 |];
               [| c 30; c 40; c 70; c 80 |];
             |]
           in
           let expected = pixelate img 1 in
           assert_equal expected img );
         ( "Test pixelate with factor 0 shouldn't change image" >:: fun _ ->
           let img =
             [|
               [| c 10; c 20; c 50; c 60 |];
               [| c 30; c 40; c 70; c 80 |];
               [| c 10; c 20; c 50; c 60 |];
               [| c 30; c 40; c 70; c 80 |];
             |]
           in
           let expected = pixelate img 0 in
           assert_equal expected img );
         ( "Test pixelate on a rectangle 3x5" >:: fun _ ->
           let img =
             [|
               [| c 10; c 20; c 30; c 40; c 50 |];
               [| c 15; c 25; c 35; c 45; c 55 |];
               [| c 60; c 70; c 80; c 90; c 100 |];
             |]
           in
           let b1 = c (70 / 4) in
           let b2 = c (150 / 4) in
           let b3_top = c (105 / 4) in
           let b4 = c (130 / 4) in
           let b5 = c (170 / 4) in
           let b6 = c (100 / 4) in
           let _ = pixelate img 2 in
           let expected =
             [|
               [| b1; b1; b2; b2; b3_top |];
               [| b1; b1; b2; b2; b3_top |];
               [| b4; b4; b5; b5; b6 |];
             |]
           in
           assert_equal expected img );
         ( "Test pixelate with a factor larger than the dimensions" >:: fun _ ->
           let img =
             [|
               [| c 10; c 10; c 10; c 10 |];
               [| c 10; c 10; c 10; c 10 |];
               [| c 10; c 10; c 10; c 10 |];
               [| c 10; c 10; c 10; c 10 |];
             |]
           in
           let _ = pixelate img 10 in
           let block1 = c (160 / 100) in
           let expected =
             [|
               [| block1; block1; block1; block1 |];
               [| block1; block1; block1; block1 |];
               [| block1; block1; block1; block1 |];
               [| block1; block1; block1; block1 |];
             |]
           in
           assert_equal expected img );
         ( "Test pixelate 3x3 with factor of 2" >:: fun _ ->
           let img =
             [|
               [| c 10; c 20; c 30 |];
               [| c 40; c 50; c 60 |];
               [| c 70; c 80; c 90 |];
             |]
           in
           let _ = pixelate img 2 in
           let block1 = c (120 / 4) in
           let block2 = c (90 / 4) in
           let block3 = c (150 / 4) in
           let block4 = c (90 / 4) in
           let expected =
             [|
               [| block1; block1; block2 |];
               [| block1; block1; block2 |];
               [| block3; block3; block4 |];
             |]
           in
           assert_equal expected img );
         ( "Test pixelate (factor 2)" >:: fun _ ->
           let img =
             [|
               [| c 10; c 20; c 50; c 60 |];
               [| c 30; c 40; c 70; c 80 |];
               [| c 10; c 20; c 50; c 60 |];
               [| c 30; c 40; c 70; c 80 |];
             |]
           in
           let _ = pixelate img 2 in
           let block1 = c 25 in
           let block2 = c 65 in
           let expected =
             [|
               [| block1; block1; block2; block2 |];
               [| block1; block1; block2; block2 |];
               [| block1; block1; block2; block2 |];
               [| block1; block1; block2; block2 |];
             |]
           in
           assert_equal expected img );
         ( "pixelate should do in-place mutation" >:: fun _ ->
           let img = [| [| c 10 |] |] in
           let result = pixelate img 0 in
           assert_bool "pixelate should mutate in place" (img == result) );
         ( "pixelate with a negative factor should return the same image"
         >:: fun _ ->
           let img = [| [| c 10 |] |] in
           let _ = pixelate img (-10) in
           let expected = [| [| c 10 |] |] in
           assert_equal img expected );
         ( "pixelate on an empty image should return the same image" >:: fun _ ->
           let img = [| [||] |] in
           let _ = pixelate img 2 in
           let expected = [| [||] |] in
           assert_equal img expected );
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
         ( "Test grayscale makes pixels gray" >:: fun _ ->
           let p1 = Graphics.rgb 10 20 30 in
           let p2 = Graphics.rgb 200 150 100 in
           let img = [| [| p1; p2 |]; [| p2; p1 |] |] in
           let result = grayscale img in
           assert_equal 2 (Array.length result);
           assert_equal 2 (Array.length result.(0));
           Array.iter
             (fun row ->
               Array.iter
                 (fun p ->
                   let rr = r p and gg = g p and bb = b p in
                   assert_bool "grayscale: r = g = b" (rr = gg && gg = bb))
                 row)
             result );
         ( "Test grayscale on already gray image is identity" >:: fun _ ->
           let img = [| [| c 10; c 20 |]; [| c 30; c 40 |] |] in
           let result = grayscale img in
           assert_equal img result );
         ( "Test grayscale is idempotent" >:: fun _ ->
           let p1 = Graphics.rgb 5 100 200 in
           let p2 = Graphics.rgb 250 10 60 in
           let img = [| [| p1; p2 |]; [| p2; p1 |] |] in
           let once = grayscale img in
           let twice = grayscale once in
           assert_equal once twice );
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
         ( "Test array subtraction" >:: fun _ ->
           let a = [| [| 5; 10; 15 |]; [| 20; 25; 30 |] |] in
           let b = [| [| 1; 2; 3 |]; [| 4; 5; 6 |] |] in
           let result = array_sub a b in
           let expected = [| [| 4; 8; 12 |]; [| 16; 20; 24 |] |] in
           assert_equal expected result );
         ( "Test array subtraction with negatives" >:: fun _ ->
           let a = [| [| 1; 2; 3 |]; [| 4; 5; 6 |] |] in
           let b = [| [| 5; 10; 15 |]; [| 20; 25; 30 |] |] in
           let result = array_sub a b in
           let expected = [| [| -4; -8; -12 |]; [| -16; -20; -24 |] |] in
           assert_equal expected result );
         ( "Test array subtraction with failure" >:: fun _ ->
           let a = [| [| 1; 2; 3; 4 |]; [| 3; 4; 5; 6 |] |] in
           let b = [| [| 1; 2; 3 |]; [| 4; 5; 6 |] |] in
           assert_raises (Failure "Array Subtraction Error!") (fun () ->
               array_sub a b) );
         ( "Test screen_to_image_coords" >:: fun _ ->
           let ix, iy = screen_to_image_coords 130 70 100 50 in
           assert_equal (30, 20) (ix, iy) );
         ( "Test is_within_bounds" >:: fun _ ->
           let w, h = (3, 2) in
           assert_bool "0,0 in-bounds" (is_within_bounds 0 0 w h);
           assert_bool "2,1 in-bounds" (is_within_bounds 2 1 w h);
           assert_bool "-1,0 out-of-bounds" (not (is_within_bounds (-1) 0 w h));
           assert_bool "3,0 out-of-bounds" (not (is_within_bounds 3 0 w h));
           assert_bool "0,2 out-of-bounds" (not (is_within_bounds 0 2 w h)) );
         ( "Test pixel_to_string" >:: fun _ ->
           let s = pixel_to_string 10 20 30 in
           assert_equal "RGB(10, 20, 30)" s );
         ( "Test replace_color simple" >:: fun _ ->
           let p1 = Graphics.rgb 10 20 30 in
           let p2 = Graphics.rgb 40 50 60 in
           let p3 = Graphics.rgb 70 80 90 in
           let img = [| [| p1; p2 |]; [| p1; p3 |] |] in
           let result = replace_color img (10, 20, 30) (0, 0, 0) in
           let black = Graphics.rgb 0 0 0 in
           let expected = [| [| black; p2 |]; [| black; p3 |] |] in
           assert_equal expected result );
         ( "Test enlarge 1x1 -> 2x2" >:: fun _ ->
           let p = c 50 in
           let img = [| [| p |] |] in
           let result = enlarge img in
           let expected = [| [| p; p |]; [| p; p |] |] in
           assert_equal expected result );
         ( "Test enlarge 2x2 -> 4x4" >:: fun _ ->
           let a = c 10 in
           let b = c 20 in
           let c_ = c 30 in
           let d = c 40 in
           let img = [| [| a; b |]; [| c_; d |] |] in
           let result = enlarge img in
           let expected =
             [|
               [| a; a; b; b |];
               [| a; a; b; b |];
               [| c_; c_; d; d |];
               [| c_; c_; d; d |];
             |]
           in
           assert_equal expected result );
         ( "Test shrink_then_enlarge_square_greyscale" >:: fun _ ->
           let img =
             [|
               [| c 10; c 10; c 50; c 50 |];
               [| c 10; c 10; c 50; c 50 |];
               [| c 80; c 80; c 120; c 120 |];
               [| c 80; c 80; c 120; c 120 |];
             |]
           in
           let shrunk = shrink img in
           let enlarged = enlarge shrunk in
           (* Should give back exactly same image *)
           assert_equal img enlarged );
         ( "array_sub simple 2x2" >:: fun _ ->
           let a = [| [| 5; 7 |]; [| 10; 0 |] |] in
           let b = [| [| 1; 2 |]; [| 3; 4 |] |] in
           let result = array_sub a b in
           let expected = [| [| 4; 5 |]; [| 7; -4 |] |] in
           assert_equal expected result );
         ( "array_sub with zeros" >:: fun _ ->
           let a = [| [| 1; 2; 3 |] |] in
           let b = [| [| 0; 0; 0 |] |] in
           let result = array_sub a b in
           let expected = [| [| 1; 2; 3 |] |] in
           assert_equal expected result );
         ( "array_sub allows negative results" >:: fun _ ->
           let a = [| [| 1; 2 |] |] in
           let b = [| [| 3; 5 |] |] in
           let result = array_sub a b in
           let expected = [| [| -2; -3 |] |] in
           assert_equal expected result );
         ( "array_sub different row counts raises error" >:: fun _ ->
           let a = [| [| 1; 2 |]; [| 3; 4 |] |] in
           let b = [| [| 1; 2 |] |] in
           assert_raises (Failure "Array Subtraction Error!") (fun () ->
               ignore (array_sub a b)) );
         ( "array_sub different column counts raises error" >:: fun _ ->
           let a = [| [| 1; 2; 3 |] |] in
           let b = [| [| 1; 2 |] |] in
           assert_raises (Failure "Array Subtraction Error!") (fun () ->
               ignore (array_sub a b)) );
         ( "Test paste 1x1 onto 2x2 at (1,1)" >:: fun _ ->
           let data = [| [| 0; 0 |]; [| 0; -1 |] |] in
           let image = [| [| 0; 0 |]; [| 0; 0 |] |] in
           let cut = [| [| 3; 1 |]; [| 1; 5 |] |] in
           let result = paste data image cut (1, 1) in
           let expected = [| [| 0; 0 |]; [| 0; 5 |] |] in
           print_endline
             (Printf.sprintf "Result: [%d,%d; %d,%d]"
                result.(0).(0)
                result.(0).(1)
                result.(1).(0)
                result.(1).(1));
           assert_equal expected result );
         ( "Test rotate_90 2x3 -> 3x2" >:: fun _ ->
           let img = [| [| 1; 2; 3 |]; [| 4; 5; 6 |] |] in
           let result = rotate_90 img in
           let expected = [| [| 4; 1 |]; [| 5; 2 |]; [| 6; 3 |] |] in
           assert_equal expected result );
         ( "Test rotate_90 2x2" >:: fun _ ->
           let img = [| [| 1; 2 |]; [| 3; 4 |] |] in
           let result = rotate_90 img in
           let expected = [| [| 3; 1 |]; [| 4; 2 |] |] in
           assert_equal expected result );
         ( "Test adjust_brightness +" >:: fun _ ->
           let img = [| [| c 10; c 20 |]; [| c 30; c 40 |] |] in
           let result = adjust_brightness img 20 in
           let expected = [| [| c 30; c 40 |]; [| c 50; c 60 |] |] in
           assert_equal expected result );
         ( "Test adjust_brightness -" >:: fun _ ->
           let img = [| [| c 100; c 120 |]; [| c 140; c 160 |] |] in
           let result = adjust_brightness img (-30) in
           let expected = [| [| c 70; c 90 |]; [| c 110; c 130 |] |] in
           assert_equal expected result );
         ( "Test adjust_brightness clamp high" >:: fun _ ->
           let img = [| [| c 240; c 250 |] |] in
           let result = adjust_brightness img 30 in
           let expected = [| [| c 255; c 255 |] |] in
           assert_equal expected result );
         ( "Test adjust_brightness clamp low" >:: fun _ ->
           let img = [| [| c 5; c 20 |] |] in
           let result = adjust_brightness img (-50) in
           let expected = [| [| c 0; c 0 |] |] in
           assert_equal expected result );
       ]

let _ = run_test_tt_main tests
