open OUnit2
open FinalProject
open ImageOps
open ImageLoader

let white = Graphics.rgb 255 255 255

let tests =
  "Image Loading and Coordinate Tests"
  >::: [
         ( "Test screen_to_image_coords - basic" >:: fun _ ->
           let result = screen_to_image_coords 100 100 50 50 in
           assert_equal (50, 50) result;
           let result = screen_to_image_coords 0 0 50 50 in
           assert_equal (-50, -50) result;
           let result = screen_to_image_coords 200 150 75 60 in
           assert_equal (125, 90) result );
         ( "Test screen_to_image_coords - negative coords" >:: fun _ ->
           let result = screen_to_image_coords 50 50 100 100 in
           assert_equal (-50, -50) result;
           let result = screen_to_image_coords 10 20 30 40 in
           assert_equal (-20, -20) result );
         ( "Test is_within_bounds - inside bounds" >:: fun _ ->
           assert_equal true (is_within_bounds 0 0 100 100);
           assert_equal true (is_within_bounds 50 50 100 100);
           assert_equal true (is_within_bounds 99 99 100 100) );
         ( "Test is_within_bounds - outside bounds" >:: fun _ ->
           assert_equal false (is_within_bounds 100 100 100 100);
           assert_equal false (is_within_bounds (-1) 50 100 100);
           assert_equal false (is_within_bounds 50 (-1) 100 100);
           assert_equal false (is_within_bounds 50 100 100 100) );
         ( "Test is_within_bounds - edge cases" >:: fun _ ->
           assert_equal true (is_within_bounds 0 0 1 1);
           assert_equal false (is_within_bounds 1 1 1 1);
           assert_equal true (is_within_bounds 0 0 1000 1000);
           assert_equal false (is_within_bounds 1000 0 1000 1000) );
         ( "Test pixel_to_string - basic RGB" >:: fun _ ->
           let result = pixel_to_string 255 0 0 in
           assert_equal "RGB(255, 0, 0)" result;
           let result = pixel_to_string 0 255 0 in
           assert_equal "RGB(0, 255, 0)" result;
           let result = pixel_to_string 128 128 128 in
           assert_equal "RGB(128, 128, 128)" result );
         ( "Test ImageLoader loads RGB24 PNG" >:: fun _ ->
           let img =
             ImageLoader.load_image_raw "../data/testData/output_rgb24.png"
           in
           let w, h = ImageLoader.image_dimensions img in
           assert_bool "width positive" (w > 0);
           assert_bool "height positive" (h > 0) );
         ( "Test ImageLoader loads RGBA32 PNG" >:: fun _ ->
           let img =
             ImageLoader.load_image_raw "../data/testData/output_rgba32.png"
           in
           let w, h = ImageLoader.image_dimensions img in
           assert_bool "width positive" (w > 0);
           assert_bool "height positive" (h > 0) );
         ( "Test ImageLoader loads Index8 PNG" >:: fun _ ->
           let img =
             ImageLoader.load_image_raw "../data/testData/output_index8.png"
           in
           let w, h = ImageLoader.image_dimensions img in
           assert_bool "width positive" (w > 0);
           assert_bool "height positive" (h > 0) );
         ( "Test image_dimensions with Cmyk32" >:: fun _ ->
           let open Camlimages in
           let w, h = (5, 6) in
           let img = Cmyk32.create w h in
           (* Fill pixels with arbitrary CMYK data *)
           for y = 0 to h - 1 do
             for x = 0 to w - 1 do
               Cmyk32.set img x y { Color.c = 0; m = 0; y = 0; k = 0 }
             done
           done;
           let result = ImageLoader.image_dimensions (Cmyk32 img) in
           assert_equal (w, h) result );
         ( "Test cmyk_pixel_to_rgb white (C=0 M=0 Y=0 K=0)" >:: fun _ ->
           let open Camlimages in
           let px = { Color.c = 0; m = 0; y = 0; k = 0 } in
           let rgb = cmyk_pixel_to_rgb px in
           assert_equal 255 rgb.Color.r;
           assert_equal 255 rgb.Color.g;
           assert_equal 255 rgb.Color.b );
         ( "Test cmyk_pixel_to_rgb black (C=0 M=0 Y=0 K=255)" >:: fun _ ->
           let open Camlimages in
           let px = { Color.c = 0; m = 0; y = 0; k = 255 } in
           let rgb = cmyk_pixel_to_rgb px in
           assert_equal 0 rgb.Color.r;
           assert_equal 0 rgb.Color.g;
           assert_equal 0 rgb.Color.b );
         ( "Test cmyk_pixel_to_rgb cyan (C=255 M=0 Y=0 K=0)" >:: fun _ ->
           let open Camlimages in
           let px = { Color.c = 255; m = 0; y = 0; k = 0 } in
           let rgb = cmyk_pixel_to_rgb px in
           assert_equal 0 rgb.Color.r;
           assert_bool "green is 255" (rgb.Color.g = 255);
           assert_bool "blue is 255" (rgb.Color.b = 255) );
       ]

let _ = run_test_tt_main tests
