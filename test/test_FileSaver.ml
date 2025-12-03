open OUnit2
open FinalProject
open FileSaver

let make_pixel_array () =
  [| [| 0x112233; 0x445566 |]; [| 0x778899; 0xAABBCC |] |]

let tests =
  "FileSaver Tests"
  >::: [
         ( "save_png_and_load" >:: fun _ ->
           let data = make_pixel_array () in
           let out = "test_saved.png" in
           (try Sys.remove out with _ -> ());
           FileSaver.save_image_to_png data out;
           let img = Images.load out [] in
           let w, h =
             match img with
             | Images.Rgb24 i -> (i.Rgb24.width, i.Rgb24.height)
             | Images.Rgba32 i ->
                 let r = Rgb24.of_rgba32 i in
                 (r.Rgb24.width, r.Rgb24.height)
             | Images.Index8 i ->
                 let r = Index8.to_rgb24 i in
                 (r.Rgb24.width, r.Rgb24.height)
             | Images.Index16 i ->
                 let r = Index16.to_rgb24 i in
                 (r.Rgb24.width, r.Rgb24.height)
             | Images.Cmyk32 i -> (i.Cmyk32.width, i.Cmyk32.height)
           in
           assert_equal 2 w;
           assert_equal 2 h;
           (try Sys.remove out with _ -> ());
           () );

       ]

let _ = run_test_tt_main tests
