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

let load_image_raw (path : string) : Images.t =
  Printf.eprintf "Loading image from: %s\n%!" path;
  Images.load path []

let image_dimensions (img : Images.t) : int * int =
  match img with
  | Rgb24 i -> (i.Rgb24.width, i.Rgb24.height)
  | Rgba32 i ->
      let rgb = Rgb24.of_rgba32 i in
      (rgb.Rgb24.width, rgb.Rgb24.height)
  | Index8 i ->
      let rgb = Index8.to_rgb24 i in
      (rgb.Rgb24.width, rgb.Rgb24.height)
  | Index16 i ->
      let rgb = Index16.to_rgb24 i in
      (rgb.Rgb24.width, rgb.Rgb24.height)
  | Cmyk32 i -> (i.Cmyk32.width, i.Cmyk32.height)

let make_graphics_image (img : Images.t) : Graphics.image =
  let rgb =
    match img with
    | Rgb24 i -> i
    | Rgba32 i -> Rgb24.of_rgba32 i
    | Index8 i -> Index8.to_rgb24 i
    | Index16 i -> Index16.to_rgb24 i
    | Cmyk32 i ->
        let w = i.Cmyk32.width and h = i.Cmyk32.height in
        let out = Rgb24.create w h in
        for yy = 0 to h - 1 do
          for xx = 0 to w - 1 do
            let px = Cmyk32.get i xx yy in
            Rgb24.set out xx yy
              (let c = float_of_int px.Color.c /. 255.0
               and m = float_of_int px.Color.m /. 255.0
               and y = float_of_int px.Color.y /. 255.0
               and k = float_of_int px.Color.k /. 255.0 in
               let conv comp ink =
                 int_of_float (255.0 *. (1.0 -. comp) *. (1.0 -. ink))
               in
               { Color.r = conv c k; Color.g = conv m k; Color.b = conv y k })
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
  Graphics.make_image data

let graphics_image_of_file (path : string) : Graphics.image * int * int =
  let img = load_image_raw path in
  let w, h = image_dimensions img in
  (make_graphics_image img, w, h)

(* Inline tests for CMYK -> RGB conversion *)
let%test "cmyk_to_rgb_white" =
  let px = { Color.c = 0; m = 0; y = 0; k = 0 } in
  let rgb = cmyk_pixel_to_rgb px in 
  rgb.Color.r = 255 && rgb.Color.g = 255 && rgb.Color.b = 255  [@coverage off]

let%test "cmyk_to_rgb_black" =
  let px = { Color.c = 0; m = 0; y = 0; k = 255 } in
  let rgb = cmyk_pixel_to_rgb px in
  rgb.Color.r = 0 && rgb.Color.g = 0 && rgb.Color.b = 0  [@coverage off]
