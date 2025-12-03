(** Convert pixel array (RGB24) to Camlimages image *)
let pixel_array_to_image (data : int array array) : Rgb24.t =
  let h = Array.length data in
  if h = 0 then failwith "Empty pixel array";
  let w = Array.length data.(0) in
  let img = Rgb24.create w h in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let pixel = data.(y).(x) in
      let r = (pixel lsr 16) land 0xFF in
      let g = (pixel lsr 8) land 0xFF in
      let b = pixel land 0xFF in
      Rgb24.set img x y { Color.r; g; b }
    done
  done;
  img

(** Save pixel data to PNG file *)
let save_image_to_png (data : int array array) (output_path : string) : unit =
  try
    let img = pixel_array_to_image data in
    Images.save output_path (Some Images.Png) [] (Images.Rgb24 img);
    Printf.printf "Image saved to %s\n" output_path;
    flush stdout
  with e ->
    Printf.printf "Error saving PNG: %s\n" (Printexc.to_string e);
    flush stdout

(** Save pixel data to JPEG file *)
let save_image_to_jpg (data : int array array) (output_path : string) : unit =
  try
    let img = pixel_array_to_image data in
    Images.save output_path (Some Images.Jpeg) [] (Images.Rgb24 img);
    Printf.printf "Image saved to %s\n" output_path;
    flush stdout
  with e ->
    Printf.printf "Error saving JPEG: %s\n" (Printexc.to_string e);
    flush stdout

(* Inline testing for pixel array to dimensions*)

let%test "pixel_array_to_image_single_pixel" =
  let data = [| [| 0x112233 |] |] in
  let img = pixel_array_to_image data in
  let px = Rgb24.get img 0 0 in
  px.Color.r = 0x11 && px.Color.g = 0x22 && px.Color.b = 0x33

let%test "pixel_array_to_image_dimensions" =
  let data =
    [|
      [| 0x000000; 0xFFFFFF |];
      [| 0x123456; 0xABCDEF |];
    |]
  in
  let img = pixel_array_to_image data in
  img.Rgb24.width = 2 && img.Rgb24.height = 2

let%test "pixel_array_to_image_empty_raises" =
  try
    let _ = pixel_array_to_image [||] in
    false
  with
  | Failure _ -> true
  | _ -> false

