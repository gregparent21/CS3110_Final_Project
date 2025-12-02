open OUnit2
open FinalProject
open ImageOps

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
       ]

let _ = run_test_tt_main tests
