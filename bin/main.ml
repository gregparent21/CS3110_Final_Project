open Graphics
open FinalProject.ImageOps

(**[graphics_image_of_file path] is a tuple of a Graphics.image and its width 
and height of given image of file path [path] *)
let graphics_image_of_file (path : string) : Graphics.image * int * int =
  Printf.eprintf "Loading image from: %s\n%!" path;
  let raw = Images.load path [] in

  (**[cmyk_pixel_to_rgb px] converts [px] of color
   formatting cmyk to rgb24 for use in Graphics library functions  *)
  let cmyk_pixel_to_rgb (px : Color.cmyk) : Color.rgb =
    (* normalize 0–255 integers to floats 0–1 *)
    let c = float_of_int px.Color.c /. 255.0
    and m = float_of_int px.Color.m /. 255.0
    and y = float_of_int px.Color.y /. 255.0
    and k = float_of_int px.Color.k /. 255.0 in
    (* Convert from subtractive (CMYK) to additive (RGB) color model *)
    let conv comp ink = int_of_float (255.0 *. (1.0 -. comp) *. (1.0 -. ink)) in
    { Color.r = conv c k; Color.g = conv m k; Color.b = conv y k }
  in


  (* Graphics library only works with rgb24 image types. We have to convert
     whatever file we loaded to Rgb24.t *)
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

  (** [usage ()] outputs the intended command line command format to run the 
  program as intended *)
let usage () =
  prerr_endline "Usage: image_viewer <image_path>";
  exit 1

(** [draw_axes img_x img_y w h] draws X and Y axes with ticks and labels *)
let draw_axes img_x img_y w h =
  set_color black;
  moveto img_x img_y;
  lineto (img_x + w) img_y; 
  moveto img_x img_y;
  lineto img_x (img_y + h); 

  let tick_spacing = 50 in
  let tick_len = 6 in

  (* X-axis ticks and labels (image-local X coordinates) *)
  let x_ticks = w / tick_spacing in
  for t = 0 to x_ticks do
    let i = t * tick_spacing in
    let x = img_x + i in
    moveto x img_y;
    lineto x (img_y + tick_len);
    moveto (x - 8) (img_y - 22);
    draw_string (string_of_int i)
  done;

  (* Y-axis ticks and labels (image-local Y coordinates) *)
  let y_ticks = h / tick_spacing in
  for t = 0 to y_ticks do
    let j = t * tick_spacing in
    let y = img_y + j in
    moveto img_x y;
    lineto (img_x - tick_len) y;
    moveto (img_x - 40) (y - 4);
    draw_string (string_of_int j)
  done

(** [handle_click img_x img_y w h] waits for mouse clicks and prints the 
    image-local coordinates of clicked pixels *)
let handle_click img_x img_y w h =
  let rec click_loop () =
    if button_down () then
      let screen_x, screen_y = mouse_pos () in
      let img_px, img_py = screen_to_image_coords screen_x screen_y img_x img_y in
      if is_within_bounds img_px img_py w h then (
        Printf.printf "Clicked at image coordinates: (%d, %d)\n" img_px img_py;
        flush stdout
      ) else
        Printf.printf "Click outside image bounds\n";
      Unix.sleepf 0.2; 
      click_loop ()
    else if key_pressed () then
      ignore (read_key ())
    else (
      Unix.sleepf 0.01;
      click_loop ())
  in
  click_loop ()

let () =
  if Array.length Sys.argv < 2 then usage ();
  let path = Sys.argv.(1) in

  if not (Sys.file_exists path) then (
    Printf.printf "File not found: %s\n" path;
    exit 1);

  open_graph " 800x600";
  set_window_title (Filename.basename path);
  auto_synchronize false;

  (* Try to load the image *)
  let img, w, h =
    try graphics_image_of_file path
    with e ->
      Printf.printf "Failed to load image: %s\n" (Printexc.to_string e);
      Printexc.print_backtrace stderr;
      exit 1
  in

  let win_w = size_x () in
  let win_h = size_y () in
  clear_graph ();
  let img_x = (win_w - w) / 2 in
  let img_y = (win_h - h) / 2 in
  draw_image img img_x img_y;

  draw_axes img_x img_y w h;
  synchronize ();

  handle_click img_x img_y w h
