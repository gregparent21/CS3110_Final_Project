open Graphics

(** Load an image file and convert it into a [Graphics.image] with its width and
    height. Supports multiple on-disk formats (RGB24, RGBA32, Index8/16, CMYK)
    by normalizing to Rgb24 then creating a Graphics.image. *)
let cmyk_pixel_to_rgb (px : Color.cmyk) : Color.rgb =
  let c = float_of_int px.Color.c /. 255.0
  and m = float_of_int px.Color.m /. 255.0
  and y = float_of_int px.Color.y /. 255.0
  and k = float_of_int px.Color.k /. 255.0 in
  let conv comp ink = int_of_float (255.0 *. (1.0 -. comp) *. (1.0 -. ink)) in
  { Color.r = conv c k; Color.g = conv m k; Color.b = conv y k }


let graphics_image_of_file (path : string) : Graphics.image * int * int =
  Printf.eprintf "Loading image from: %s\n%!" path;
  let raw = Images.load path [] in

  let rgb =
    match raw with
    | Images.Rgb24 img ->
        Printf.printf "Detected format: Rgb24\n%!";
        img
    | Images.Index8 img ->
        Printf.printf "Detected format: Index8 -> converting to Rgb24\n%!";
        Index8.to_rgb24 img
    | Images.Index16 img ->
        Printf.printf "Detected format: Index16 -> converting to Rgb24\n%!";
        Index16.to_rgb24 img
    | Images.Rgba32 img ->
        Printf.printf "Detected format: Rgba32 -> converting to Rgb24\n%!";
        Rgb24.of_rgba32 img
    | Images.Cmyk32 img ->
        Printf.printf "Detected format: Cmyk32 -> Rgb24 (manual)\n%!";
        let w = img.Cmyk32.width and h = img.Cmyk32.height in
        let out = Rgb24.create w h in
        for yy = 0 to h - 1 do
          for xx = 0 to w - 1 do
            let px = Cmyk32.get img xx yy in
            Rgb24.set out xx yy (cmyk_pixel_to_rgb px)
          done
        done;
        out
  in

  let w = rgb.Rgb24.width in
  let h = rgb.Rgb24.height in

  let data = Array.make_matrix h w 0 in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let { Color.r; g; b } = Rgb24.get rgb x y in
      data.(y).(x) <- (r lsl 16) lor (g lsl 8) lor b
    done
  done;

  (Graphics.make_image data, w, h)

(* Inline tests for CMYK -> RGB conversion logic *)

let%test "cmyk_white_to_rgb_white" =
  let rgb = cmyk_pixel_to_rgb { Color.c = 0; m = 0; y = 0; k = 0 } in
  rgb.Color.r = 255 && rgb.Color.g = 255 && rgb.Color.b = 255

let%test "cmyk_black_to_rgb_black" =
  let rgb = cmyk_pixel_to_rgb { Color.c = 0; m = 0; y = 0; k = 255 } in
  rgb.Color.r = 0 && rgb.Color.g = 0 && rgb.Color.b = 0

let%test "cmyk_cyan_to_rgb" =
  let rgb = cmyk_pixel_to_rgb { Color.c = 255; m = 0; y = 0; k = 0 } in
  rgb.Color.r = 0 && rgb.Color.g = 255 && rgb.Color.b = 255
