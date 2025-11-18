open Graphics
open FinalProject.ImageOps

(**[graphics_image_of_file path] is a tuple of a Graphics.image and its width
   and height of given image of file path [path] *)
let graphics_image_of_file (path : string) : Graphics.image * int * int =
  Printf.eprintf "Loading image from: %s\n%!" path;
  let raw = Images.load path [] in

  (*[cmyk_pixel_to_rgb] converts `px` of color formatting cmyk to rgb24 for use
    in Graphics library functions.*)
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

(** [draw_button x y w h label selected] draws a button with text. Fills with
    light gray if selected, white otherwise. *)
let draw_button x y w h label selected =
  if selected then set_color (rgb 200 200 200) else set_color white;
  fill_rect x y w h;
  set_color black;
  draw_rect x y w h;
  moveto (x + 5) (y + (h / 2) - 4);
  draw_string label

(** [is_point_in_rect px py x y w h] checks if point (px, py) is inside rect *)
let is_point_in_rect px py x y w h =
  px >= x && px <= x + w && py >= y && py <= y + h

(** [draw_toolbar win_w win_h toolbar_x current_tool] draws tool buttons on the
    right side *)
let draw_toolbar win_w win_h toolbar_x current_tool =
  set_color (rgb 220 220 220);
  fill_rect toolbar_x 0 (win_w - toolbar_x) win_h;
  set_color black;
  draw_rect toolbar_x 0 (win_w - toolbar_x) win_h;

  let button_y = win_h - 60 in
  let button_w = 90 in
  let button_h = 40 in

  (* Draw Cut button *)
  draw_button toolbar_x button_y button_w button_h "Cut" (current_tool = "cut");
  moveto (toolbar_x + 5) (button_y + button_h + 5);
  draw_string "Click to select cut tool";

  (*Draw Compress button *)
  let button_compress_y = win_h - 120 in
  draw_button toolbar_x button_compress_y button_w button_h "Compress"
    (current_tool = "compress");
  moveto (toolbar_x + 5) (button_y + button_h - 60);
  draw_string "Click to compress image"

(**[handle_buttons] allows the user to interact with buttons and alter an image
   accordingly.*)
let handle_buttons img_x img_y w h img_data toolbar_x =
  let clicked_points = ref [] in
  (*These lengths defined below are from [draw_toolbar]. Must accurately reflect
    any of the pre-defined values.*)
  let button_width = 90 in
  let button_height = 40 in
  let cut_y = size_y () - 60 in
  let compress_y = size_y () - 120 in
  let rec event_loop current_tool =
    let screen_x, screen_y = mouse_pos () in
    if button_down () then
      if screen_x >= toolbar_x then
        if
          is_point_in_rect screen_x screen_y toolbar_x cut_y button_width
            button_height
        then (
          Printf.printf
            "Cut tool selected! Click on image to set points. Press 'c' to \
             apply cut, 'r' to reset.\n";
          flush stdout;
          Unix.sleepf 0.2;
          event_loop "cut")
        else if
          is_point_in_rect screen_x screen_y toolbar_x compress_y button_width
            button_height
        then (
          Printf.printf "Compress tool selected! Compressing image.\n";
          flush stdout;
          Unix.sleepf 0.2;
          event_loop "compress")
        else (
          Unix.sleepf 0.2;
          event_loop current_tool)
      else
        (* Click on image *)
        let img_px, img_py =
          screen_to_image_coords screen_x screen_y img_x img_y
        in
        if current_tool = "cut" && is_within_bounds img_px img_py w h then (
          clicked_points := (img_px, img_py) :: !clicked_points;
          Printf.printf "Point added: (%d, %d). Total: %d\n" img_px img_py
            (List.length !clicked_points);
          flush stdout;
          Unix.sleepf 0.2;
          event_loop current_tool)
        else (
          Printf.printf "No tool selected or click outside bounds\n";
          Unix.sleepf 0.2;
          event_loop current_tool)
    else if key_pressed () then
      let key = read_key () in
      if key = 'c' && current_tool = "cut" && List.length !clicked_points > 2
      then (
        (* Apply cut *)
        let cut_data = cut_advanced img_data !clicked_points in
        Printf.printf "Cut applied with %d points!\n"
          (List.length !clicked_points);
        flush stdout;

        (* Redraw *)
        let new_img = Graphics.make_image cut_data in
        clear_graph ();
        draw_image new_img img_x img_y;
        draw_axes img_x img_y w h;
        draw_toolbar (size_x ()) (size_y ()) toolbar_x current_tool;
        synchronize ();

        (* Reset and continue *)
        clicked_points := [];
        Printf.printf "Points reset. Select a tool and continue.\n";
        flush stdout;
        event_loop "")
      else if key = 'r' then (
        clicked_points := [];
        Printf.printf "Points reset.\n";
        flush stdout;
        event_loop current_tool)
      else if key = 'q' then Printf.printf "Exiting.\n"
      else event_loop current_tool
    else (
      Unix.sleepf 0.01;
      event_loop current_tool)
  in
  event_loop ""

(** [handle_click img_x img_y w h] waits for mouse clicks and prints the
    image-local coordinates of clicked pixels *)
let handle_click img_x img_y w h =
  let rec click_loop () =
    if button_down () then (
      let screen_x, screen_y = mouse_pos () in
      let img_px, img_py =
        screen_to_image_coords screen_x screen_y img_x img_y
      in
      if is_within_bounds img_px img_py w h then (
        Printf.printf "Clicked at image coordinates: (%d, %d)\n" img_px img_py;
        flush stdout)
      else Printf.printf "Click outside image bounds\n";
      Unix.sleepf 0.2;
      click_loop ())
    else if key_pressed () then ignore (read_key ())
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
  let toolbar_x = 650 in

  clear_graph ();
  let img_x = (toolbar_x - w) / 2 in
  let img_y = (win_h - h) / 2 in
  draw_image img img_x img_y;

  (* Convert image to pixel array for cut operations *)
  let data = Graphics.dump_image img in

  draw_axes img_x img_y w h;
  draw_toolbar win_w win_h toolbar_x "";
  synchronize ();

  Printf.printf
    "Click the 'Cut' button to select the cut tool, then click on image to set \
     polygon points.\n\
     Click the 'Compress' button to select the compression tool.\n";
  Printf.printf
    "Press 'c' to apply cut, 'r' to reset points, 's' to compress the image, \
     'q' to quit.\n";
  flush stdout;

  handle_buttons img_x img_y w h data toolbar_x
