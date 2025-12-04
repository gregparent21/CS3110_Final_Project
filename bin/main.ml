open Graphics
open FinalProject.ImageOps
open FinalProject.ImageLoader

(* Enable backtraces for better error diagnostics. *)
let () = Printexc.record_backtrace true

(* Bottom message panel height (pixels) *)
let message_panel_h = 100

(* Message buffer: keep most recent lines at bottom panel *)
let messages : string list ref = ref []
let pixel_fact = ref 2

(* Record type to capture everything that changes when we edit *)
type editor_state = {
  pixels : int array array;
  img_x : int;
  img_y : int;
  w : int;
  h : int;
}

(* Generic exception logger that prints both to stderr and the message panel. *)
let log_exception context exn =
  match exn with
  | Graphics.Graphic_failure msg ->
      (* Special case: graphics screen is gone; do NOT attempt drawing. *)
      prerr_endline ("Graphics error in " ^ context ^ ": " ^ msg)
  | _ -> (
      let exn_str = Printexc.to_string exn in
      let msg = "Unexpected error in " ^ context ^ ": " ^ exn_str in
      prerr_endline msg;
      try
        (* Try to log into the message panel. *)
        messages := msg :: !messages;
        let max_lines = 6 in
        if List.length !messages > max_lines then (
          let rec take n l =
            if n <= 0 then []
            else
              match l with
              | [] -> []
              | x :: xs -> x :: take (n - 1) xs
          in
          messages := take max_lines !messages;
          let win_w = try size_x () with _ -> 0 in
          if win_w > 0 then (
            let panel_h = message_panel_h in
            set_color (rgb 245 245 245);
            fill_rect 0 0 win_w panel_h;
            set_color black;
            draw_rect 0 0 win_w panel_h;
            let line_h = 14 in
            let margin = 8 in
            let rec draw_lines ls idx =
              match ls with
              | [] -> ()
              | hd :: tl ->
                  let y = margin + (idx * line_h) in
                  moveto margin y;
                  draw_string hd;
                  draw_lines tl (idx + 1)
            in
            let to_draw = List.rev !messages in
            draw_lines to_draw 0;
            synchronize ()))
      with _ -> ())

let draw_message_panel win_w _panel_h =
  let panel_h = message_panel_h in
  set_color (rgb 245 245 245);
  fill_rect 0 0 win_w panel_h;
  set_color black;
  draw_rect 0 0 win_w panel_h;
  let line_h = 14 in
  let margin = 8 in
  let rec draw_lines ls idx =
    match ls with
    | [] -> ()
    | hd :: tl ->
        let y = margin + (idx * line_h) in
        moveto margin y;
        draw_string hd;
        draw_lines tl (idx + 1)
  in
  (* draw most recent lines first at bottom of panel: reverse messages so newest
     last printed at top of panel *)
  let to_draw = List.rev !messages in
  draw_lines to_draw 0

let add_message s =
  (* keep max 6 lines *)
  messages := s :: !messages;
  let max_lines = 6 in
  if List.length !messages > max_lines then
    let rec take n l =
      if n <= 0 then []
      else
        match l with
        | [] -> []
        | x :: xs -> x :: take (n - 1) xs
    in
    messages := take max_lines !messages
  else ();
  (* Attempt immediate render if graphics is initialized *)
  try
    let win_w = size_x () in
    draw_message_panel win_w message_panel_h;
    synchronize ()
  with _ -> ()

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
let draw_toolbar win_w win_h toolbar_x current_tool brightness_level =
  set_color (rgb 220 220 220);
  (* leave space at bottom for the message panel *)
  fill_rect toolbar_x message_panel_h (win_w - toolbar_x)
    (win_h - message_panel_h);
  set_color black;
  draw_rect toolbar_x message_panel_h (win_w - toolbar_x)
    (win_h - message_panel_h);

  let button_y = win_h - 60 in
  let button_w = 120 in
  let button_h = 40 in

  draw_button toolbar_x button_y button_w button_h "Fill" (current_tool = "fill");
  moveto (toolbar_x + 5) (button_y + button_h + 5);

  let button_adv_y = win_h - 120 in
  draw_button toolbar_x button_adv_y button_w button_h "Cut"
    (current_tool = "cut");
  moveto (toolbar_x + 5) (button_adv_y + button_h + 5);

  (* Draw Paste button *)
  let button_paste_y = win_h - 180 in
  draw_button toolbar_x button_paste_y button_w button_h "Paste"
    (current_tool = "paste");
  moveto (toolbar_x + 5) (button_paste_y + button_h + 5);

  (* Draw - / + side by side *)
  let button_zoom_y = win_h - 240 in
  let half_w = (button_w / 2) - 2 in

  draw_button toolbar_x button_zoom_y half_w button_h "-"
    (current_tool = "zoom_out");
  moveto (toolbar_x + 5) (button_zoom_y + button_h + 5);

  let zoom_in_x = toolbar_x + half_w + 4 in
  draw_button zoom_in_x button_zoom_y half_w button_h "+"
    (current_tool = "zoom_in");
  moveto (zoom_in_x + 5) (button_zoom_y + button_h + 5);

  (* Draw Invert & Grayscale on the same row *)
  let button_invert_y = win_h - 300 in
  let gray_x = toolbar_x + button_w + 10 in

  (* Invert on left *)
  draw_button toolbar_x button_invert_y button_w button_h "Invert"
    (current_tool = "invert");
  moveto (toolbar_x + 5) (button_invert_y + button_h + 5);

  (* Grayscale on right *)
  draw_button gray_x button_invert_y button_w button_h "Grayscale"
    (current_tool = "grayscale");
  moveto (gray_x + 5) (button_invert_y + button_h + 5);

  (* Draw Mirror + Rotate next to each other *)
  let button_mirror_y = win_h - 360 in
  let spacing = 10 in
  let rotate_x = toolbar_x + button_w + spacing in

  draw_button toolbar_x button_mirror_y button_w button_h "Mirror"
    (current_tool = "mirror");
  moveto (toolbar_x + 5) (button_mirror_y + button_h + 5);

  draw_button rotate_x button_mirror_y button_w button_h "Rotate"
    (current_tool = "rotate");
  moveto (rotate_x + 5) (button_mirror_y + button_h + 5);

  (* Draw Crop *)
  let button_crop_y = win_h - 420 in
  draw_button toolbar_x button_crop_y button_w button_h "Crop"
    (current_tool = "crop");
  moveto (toolbar_x + 5) (button_crop_y + button_h + 5);

  (* Draw Pixelate *)
  let button_pixelate = win_h - 480 in
  draw_button toolbar_x button_pixelate button_w button_h "Pixelate"
    (current_tool = "pixelate");
  moveto (toolbar_x + 5) (button_pixelate + button_h + 5);

  (* Draw Save button *)
  let button_save_y = win_h - 540 in
  draw_button toolbar_x button_save_y button_w button_h "Save"
    (current_tool = "save");
  moveto (toolbar_x + 5) (button_save_y + button_h + 5);

  (* Draw Reset button *)
  let button_reset_y = win_h - 600 in
  draw_button toolbar_x button_reset_y button_w button_h "Reset"
    (current_tool = "reset");
  moveto (toolbar_x + 5) (button_reset_y + button_h + 5);

  (* Brightness slider on right *)
  let slider_width = 20 in
  let slider_margin = 10 in
  let slider_x = win_w - slider_width - slider_margin in
  let slider_y = message_panel_h + 40 in
  let slider_h = win_h - message_panel_h - 80 in

  set_color (rgb 230 230 230);
  fill_rect slider_x slider_y slider_width slider_h;
  set_color black;
  draw_rect slider_x slider_y slider_width slider_h;

  let zero_y = slider_y + (slider_h / 2) in
  moveto (slider_x - 5) zero_y;
  lineto (slider_x + slider_width + 5) zero_y;

  let max_level = 255 in
  let level =
    if brightness_level < -max_level then -max_level
    else if brightness_level > max_level then max_level
    else brightness_level
  in
  let t = float_of_int (level + max_level) /. float_of_int (2 * max_level) in
  let knob_y = slider_y + int_of_float (t *. float_of_int slider_h) in

  set_color (rgb 255 165 0);
  fill_circle (slider_x + (slider_width / 2)) knob_y 5;

  set_color black;
  let label = "Brt" in
  let tw, _th = text_size label in
  moveto (slider_x + (slider_width / 2) - (tw / 2)) (slider_y + slider_h + 5);
  draw_string label

(** [redraw img img_x img_y w h toolbar_x current_tool] is a helper function
    that redraws the image after applying the cut. *)
let redraw img img_x img_y w h toolbar_x current_tool brightness_level =
  clear_graph ();
  draw_image img img_x img_y;
  draw_axes img_x img_y w h;
  draw_toolbar (size_x ()) (size_y ()) toolbar_x current_tool brightness_level;
  draw_message_panel (size_x ()) message_panel_h;
  synchronize ()

(**[handle_buttons] allows the user to interact with buttons and alter an image
   accordingly.*)
let handle_buttons img_x img_y w h img_data toolbar_x =
  let clicked_points = ref [] in
  let fill_color = ref (Graphics.rgb 0 0 0) in
  let crop_points = ref [] in
  let crop_confirm_mode = ref false in
  let zoom_level = ref 0 in
  let zoom_base = ref (Array.map Array.copy !img_data) in
  let brightness_level = ref 0 in

  let make_display_image () =
    let pixels =
      if !brightness_level = 0 then !img_data
      else adjust_brightness !img_data !brightness_level
    in
    Graphics.make_image pixels
  in

  let rec apply_zoom n data =
    if n = 0 then data
    else if n > 0 then apply_zoom (n - 1) (enlarge data)
    else (* n < 0 *) apply_zoom (n + 1) (shrink data)
  in

  (* Remember original image + geometry for reset *)
  let original_data = Array.map Array.copy !img_data in
  let original_w = w in
  let original_h = h in
  let original_img_x = img_x in
  let original_img_y = img_y in

  (*These lengths defined below are from [draw_toolbar]. Must accurately reflect
    any of the pre-defined values.*)
  let button_width = 120 in
  let button_height = 40 in
  (* Button Y positions must match draw_toolbar layout *)
  let fill_y = size_y () - 60 in
  let cut_y = size_y () - 120 in
  let paste_y = size_y () - 180 in
  let zoom_y = size_y () - 240 in
  let invert_y = size_y () - 300 in
  let gray_x = toolbar_x + button_width + 10 in
  let mirror_y = size_y () - 360 in
  let rotate_x = toolbar_x + button_width + 10 in
  let rotate_y = mirror_y in
  let crop_y = size_y () - 420 in
  let pixelate_y = size_y () - 480 in
  let save_y = size_y () - 540 in
  let reset_y = size_y () - 600 in
  let slider_width = 20 in
  let slider_margin = 10 in
  let slider_x = size_x () - slider_width - slider_margin in
  let slider_y = message_panel_h + 40 in
  let slider_h = size_y () - message_panel_h - 80 in

  let prev_cut = ref (Array.make_matrix 0 0 0) in
  (* Track current image position and size, starting from initial *)
  let img_x_ref = ref img_x in
  let img_y_ref = ref img_y in
  let w_ref = ref w in
  let h_ref = ref h in

  (* Restore functionality*)
  let undo_stack : editor_state list ref = ref [] in
  let redo_stack : editor_state list ref = ref [] in

  let current_state () =
    {
      pixels = Array.map Array.copy !img_data;
      img_x = !img_x_ref;
      img_y = !img_y_ref;
      w = !w_ref;
      h = !h_ref;
    }
  in

  let restore_state st =
    img_data := Array.map Array.copy st.pixels;
    img_x_ref := st.img_x;
    img_y_ref := st.img_y;
    w_ref := st.w;
    h_ref := st.h
  in

  let push_undo () =
    undo_stack := current_state () :: !undo_stack;
    redo_stack := []
    (* clear redo on new action *)
  in

  let rec event_loop current_tool =
    try
      let screen_x, screen_y = mouse_pos () in
      let cur_w = !w_ref in
      let cur_h = !h_ref in
      if button_down () then
        if !crop_confirm_mode then (
          Unix.sleepf 0.2;
          event_loop current_tool)
        else if screen_x >= toolbar_x then
          if
            screen_x >= slider_x
            && screen_x <= slider_x + slider_width
            && screen_y >= slider_y
            && screen_y <= slider_y + slider_h
          then
            let max_level = 255 in
            let rel =
              float_of_int (screen_y - slider_y) /. float_of_int slider_h
            in
            (* map rel ∈ [0,1] to [-max_level, max_level] *)
            let level =
              int_of_float (rel *. float_of_int (2 * max_level)) - max_level
            in
            (* clamp *)
            let level =
              if level < -max_level then -max_level
              else if level > max_level then max_level
              else level
            in

            if level <> !brightness_level then (
              brightness_level := level;

              let win_w = size_x () in
              let win_h = size_y () in
              let new_img = make_display_image () in
              clear_graph ();
              draw_image new_img !img_x_ref !img_y_ref;
              draw_axes !img_x_ref !img_y_ref !w_ref !h_ref;
              draw_toolbar win_w win_h toolbar_x current_tool !brightness_level;
              draw_message_panel win_w message_panel_h;
              synchronize ();
              Unix.sleepf 0.05;
              event_loop current_tool)
            else (
              Unix.sleepf 0.05;
              event_loop current_tool)
          else if
            is_point_in_rect screen_x screen_y toolbar_x cut_y button_width
              button_height
          then (
            add_message "Press 'c' to cut. Press 'r' to reset.";
            add_message
              "Cut tool selected! Click two opposite corners to a cut a square \
               or three or more points to cut a polygon";
            flush stdout;
            Unix.sleepf 0.2;
            event_loop "cut")
          else if
            is_point_in_rect screen_x screen_y toolbar_x fill_y button_width
              button_height
          then (
            add_message
              "Press 'f' to apply or 'r' to reset. Enter fill color (R G B) in \
               terminal:";
            add_message
              "Fill tool selected! Click two opposite corners to fill a square \
               or three or more points to fill a polygon.";
            add_message
              "Please enter three integers between 0 and 255 in the terminal.";
            let input = read_line () in
            let rgb_vals =
              try List.map int_of_string (String.split_on_char ' ' input)
              with Failure _ ->
                add_message "Invalid input. Using default color (0, 0, 0).";
                [ 0; 0; 0 ]
            in
            fill_color :=
              Graphics.rgb
                (List.nth rgb_vals 0 |> min 255 |> max 0)
                (List.nth rgb_vals 1 |> min 255 |> max 0)
                (List.nth rgb_vals 2 |> min 255 |> max 0);
            flush stdout;
            Unix.sleepf 0.2;
            event_loop "fill")
          else if
            is_point_in_rect screen_x screen_y toolbar_x paste_y button_width
              button_height
          then (
            add_message "Press 'p' to apply the paste and 'r' to reset.";
            add_message
              "Paste tool selected! Select the bottom left endpoint where you \
               would like to paste the previous cut.";
            Unix.sleepf 0.2;
            event_loop "paste")
          else if
            is_point_in_rect screen_x screen_y toolbar_x zoom_y
              (button_width / 2) button_height
          then
            if !zoom_level <= -3 then (
              add_message "Cannot shrink further (zoom limit reached).";
              Unix.sleepf 0.2;
              event_loop current_tool)
            else (
              if !zoom_level = 0 then
                zoom_base := Array.map Array.copy !img_data;

              zoom_level := !zoom_level - 1;
              push_undo ();
              img_data := apply_zoom !zoom_level !zoom_base;

              let new_h = Array.length !img_data in
              let new_w = if new_h = 0 then 0 else Array.length !img_data.(0) in
              h_ref := new_h;
              w_ref := new_w;

              let win_w = size_x () in
              let win_h = size_y () in
              img_x_ref := (toolbar_x - !w_ref) / 2;
              img_y_ref :=
                message_panel_h + ((win_h - message_panel_h - !h_ref) / 2);

              let new_img = make_display_image () in
              clear_graph ();
              draw_image new_img !img_x_ref !img_y_ref;
              draw_axes !img_x_ref !img_y_ref !w_ref !h_ref;
              draw_toolbar win_w win_h toolbar_x "zoom_out" !brightness_level;
              draw_message_panel win_w message_panel_h;
              synchronize ();
              Unix.sleepf 0.2;
              event_loop "zoom_out")
          else if
            is_point_in_rect screen_x screen_y
              (toolbar_x + (button_width / 2) + 4)
              zoom_y (button_width / 2) button_height
          then
            if !zoom_level >= 3 then (
              add_message "Cannot enlarge further (zoom limit reached).";
              Unix.sleepf 0.2;
              event_loop current_tool)
            else (
              if !zoom_level = 0 then
                zoom_base := Array.map Array.copy !img_data;

              zoom_level := !zoom_level + 1;
              push_undo ();
              img_data := apply_zoom !zoom_level !zoom_base;

              let new_h = Array.length !img_data in
              let new_w = if new_h = 0 then 0 else Array.length !img_data.(0) in
              h_ref := new_h;
              w_ref := new_w;

              let win_w = size_x () in
              let win_h = size_y () in
              img_x_ref := (toolbar_x - !w_ref) / 2;
              img_y_ref :=
                message_panel_h + ((win_h - message_panel_h - !h_ref) / 2);

              let new_img = make_display_image () in
              clear_graph ();
              draw_image new_img !img_x_ref !img_y_ref;
              draw_axes !img_x_ref !img_y_ref !w_ref !h_ref;
              draw_toolbar win_w win_h toolbar_x "zoom_in" !brightness_level;
              draw_message_panel win_w message_panel_h;
              synchronize ();
              Unix.sleepf 0.2;
              event_loop "zoom_in")
          else if
            is_point_in_rect screen_x screen_y toolbar_x invert_y button_width
              button_height
          then (
            (* Apply inversion to the current pixel data *)
            push_undo ();
            img_data := invert_colors !img_data;
            let new_img = make_display_image () in
            (* Redraw window with inverted image using current origin/size *)
            let win_w = size_x () in
            let win_h = size_y () in
            clear_graph ();
            draw_image new_img !img_x_ref !img_y_ref;
            draw_axes !img_x_ref !img_y_ref !w_ref !h_ref;
            draw_toolbar win_w win_h toolbar_x "invert" !brightness_level;
            draw_message_panel win_w message_panel_h;
            synchronize ();
            Unix.sleepf 0.2;
            event_loop "invert")
          else if
            is_point_in_rect screen_x screen_y gray_x invert_y button_width
              button_height
          then (
            push_undo ();
            img_data := grayscale !img_data;
            let new_img = make_display_image () in

            let win_w = size_x () in
            let win_h = size_y () in
            clear_graph ();
            draw_image new_img !img_x_ref !img_y_ref;
            draw_axes !img_x_ref !img_y_ref !w_ref !h_ref;
            draw_toolbar win_w win_h toolbar_x "grayscale" !brightness_level;
            draw_message_panel win_w message_panel_h;
            synchronize ();
            Unix.sleepf 0.2;
            event_loop "grayscale")
          else if
            is_point_in_rect screen_x screen_y toolbar_x mirror_y button_width
              button_height
          then (
            add_message "Mirror tool selected! Flipping horizontally.";
            push_undo ();
            img_data := flip_horizontal !img_data;
            let new_img = make_display_image () in
            let win_w = size_x () in
            let win_h = size_y () in
            clear_graph ();
            draw_image new_img !img_x_ref !img_y_ref;
            draw_axes !img_x_ref !img_y_ref !w_ref !h_ref;
            draw_toolbar win_w win_h toolbar_x "mirror" !brightness_level;
            draw_message_panel win_w message_panel_h;
            synchronize ();
            Unix.sleepf 0.2;
            event_loop "mirror")
          else if
            is_point_in_rect screen_x screen_y rotate_x rotate_y button_width
              button_height
          then (
            push_undo ();
            img_data := rotate_90 !img_data;

            let new_h = Array.length !img_data in
            let new_w = if new_h = 0 then 0 else Array.length !img_data.(0) in
            h_ref := new_h;
            w_ref := new_w;
            let win_w = size_x () in
            let win_h = size_y () in
            img_x_ref := (toolbar_x - !w_ref) / 2;
            img_y_ref :=
              message_panel_h + ((win_h - message_panel_h - !h_ref) / 2);
            let new_img = make_display_image () in
            clear_graph ();
            draw_image new_img !img_x_ref !img_y_ref;
            draw_axes !img_x_ref !img_y_ref !w_ref !h_ref;
            draw_toolbar win_w win_h toolbar_x "rotate" !brightness_level;
            draw_message_panel win_w message_panel_h;
            synchronize ();
            Unix.sleepf 0.2;
            event_loop "rotate")
          else if
            is_point_in_rect screen_x screen_y toolbar_x crop_y button_width
              button_height
          then (
            crop_points := [];
            add_message "Crop tool selected! Click two corners.";
            Unix.sleepf 0.2;
            event_loop "crop")
          else if
            is_point_in_rect screen_x screen_y toolbar_x pixelate_y button_width
              button_height
          then (
            push_undo ();
            img_data := pixelate !img_data !pixel_fact;

            pixel_fact := !pixel_fact + 1;

            (* update current width/height from the new data *)
            let new_h = Array.length !img_data in
            let new_w = if new_h = 0 then 0 else Array.length !img_data.(0) in
            h_ref := new_h;
            w_ref := new_w;

            (* recenter image in the left area *)
            let win_w = size_x () in
            let win_h = size_y () in
            img_x_ref := (toolbar_x - !w_ref) / 2;
            img_y_ref :=
              message_panel_h + ((win_h - message_panel_h - !h_ref) / 2);
            let new_img = make_display_image () in
            clear_graph ();
            draw_image new_img !img_x_ref !img_y_ref;
            draw_axes !img_x_ref !img_y_ref !w_ref !h_ref;
            draw_toolbar win_w win_h toolbar_x "pixelate" !brightness_level;
            draw_message_panel win_w message_panel_h;
            synchronize ();
            Unix.sleepf 0.2;
            event_loop "pixelate")
          else if
            is_point_in_rect screen_x screen_y toolbar_x save_y button_width
              button_height
          then (
            add_message
              "Save tool selected! Enter filename (without extension): ";
            let filename = read_line () in
            add_message "Saving as PNG...";

            let pixels_to_save =
              if !brightness_level = 0 then !img_data
              else adjust_brightness !img_data !brightness_level
            in
            FinalProject.FileSaver.save_image_to_png pixels_to_save
              (filename ^ ".png");

            add_message (Printf.sprintf "Image saved as %s.png" filename);
            Unix.sleepf 0.2;
            event_loop "save")
          else if
            is_point_in_rect screen_x screen_y toolbar_x reset_y button_width
              button_height
          then (
            (* Restore original pixel data and geometry *)
            push_undo ();
            img_data := Array.map Array.copy original_data;
            w_ref := original_w;
            h_ref := original_h;
            img_x_ref := original_img_x;
            img_y_ref := original_img_y;
            zoom_level := 0;
            zoom_base := Array.map Array.copy !img_data;
            pixel_fact := 2;

            let win_w = size_x () in
            let win_h = size_y () in
            brightness_level := 0;
            let new_img = make_display_image () in
            clear_graph ();
            draw_image new_img !img_x_ref !img_y_ref;
            draw_axes !img_x_ref !img_y_ref !w_ref !h_ref;
            draw_toolbar win_w win_h toolbar_x "reset" !brightness_level;
            draw_message_panel win_w message_panel_h;
            add_message "Image reset to original.";
            synchronize ();
            Unix.sleepf 0.2;
            event_loop "reset")
          else (
            Unix.sleepf 0.2;
            event_loop current_tool)
        else
          (* Click on image *)
          let img_px, img_py =
            screen_to_image_coords screen_x screen_y !img_x_ref !img_y_ref
          in
          if
            (current_tool = "cut" || current_tool = "fill"
           || current_tool = "paste")
            && is_within_bounds img_px img_py cur_w cur_h
          then (
            clicked_points := (img_px, img_py) :: !clicked_points;
            (*add_message (Printf.sprintf "Point added: (%d, %d). Total: %d"
              img_px img_py (List.length !clicked_points));*)
            Unix.sleepf 0.2;
            event_loop current_tool)
          else if
            current_tool = "crop" && (not !crop_confirm_mode)
            && is_within_bounds img_px img_py cur_w cur_h
          then (
            crop_points := (img_px, img_py) :: !crop_points;

            (* Small red dot at corner*)
            set_color (rgb 255 0 0);
            fill_circle screen_x screen_y 4;
            synchronize ();

            if List.length !crop_points = 2 then (
              crop_confirm_mode := true;

              let p1, p2 =
                match !crop_points with
                | [ a; b ] -> (a, b)
                | _ -> failwith "impossible crop_points length"
              in

              (* redraw image + rectangle preview *)
              let win_w = size_x () in
              let win_h = size_y () in
              let img = make_display_image () in

              clear_graph ();
              draw_image img !img_x_ref !img_y_ref;
              draw_axes !img_x_ref !img_y_ref !w_ref !h_ref;
              draw_toolbar win_w win_h toolbar_x "crop" !brightness_level;

              let sx1 = !img_x_ref + fst p1 in
              let sy1 = !img_y_ref + snd p1 in
              let sx2 = !img_x_ref + fst p2 in
              let sy2 = !img_y_ref + snd p2 in
              let left = min sx1 sx2 in
              let right = max sx1 sx2 in
              let bottom = min sy1 sy2 in
              let top = max sy1 sy2 in

              set_color (rgb 255 0 0);
              draw_rect left bottom (right - left) (top - bottom);

              draw_message_panel win_w message_panel_h;
              add_message "Press 'c' to confirm crop, 'r' to cancel.";
              synchronize ();

              event_loop "crop")
            else (
              Unix.sleepf 0.2;
              event_loop current_tool))
          else (
            add_message "No tool selected or click outside bounds";
            Unix.sleepf 0.2;
            event_loop current_tool)
      else if key_pressed () then
        let key = read_key () in
        if key = 'c' && current_tool = "crop" && !crop_confirm_mode then (
          match List.rev !crop_points with
          | p1 :: p2 :: _ ->
              push_undo ();
              img_data := crop !img_data p1 p2;

              let new_h = Array.length !img_data in
              let new_w = Array.length !img_data.(0) in
              h_ref := new_h;
              w_ref := new_w;

              let win_w = size_x () in
              let win_h = size_y () in
              img_x_ref := (toolbar_x - !w_ref) / 2;
              img_y_ref :=
                message_panel_h + ((win_h - message_panel_h - !h_ref) / 2);

              let new_img = make_display_image () in
              clear_graph ();
              draw_image new_img !img_x_ref !img_y_ref;
              draw_axes !img_x_ref !img_y_ref !w_ref !h_ref;
              draw_toolbar win_w win_h toolbar_x "crop" !brightness_level;
              draw_message_panel win_w message_panel_h;
              synchronize ();

              crop_points := [];
              crop_confirm_mode := false;
              event_loop ""
          | _ ->
              crop_points := [];
              crop_confirm_mode := false;
              event_loop "crop")
        else if key = 'r' && current_tool = "crop" && !crop_confirm_mode then (
          (* cancel any pending crop *)
          crop_points := [];
          crop_confirm_mode := false;
          let win_w = size_x () in
          let win_h = size_y () in
          let img = make_display_image () in
          clear_graph ();
          draw_image img !img_x_ref !img_y_ref;
          draw_axes !img_x_ref !img_y_ref !w_ref !h_ref;
          draw_toolbar win_w win_h toolbar_x "crop" !brightness_level;
          draw_message_panel win_w message_panel_h;
          synchronize ();
          event_loop "crop")
        else if key = 'u' then (
          match !undo_stack with
          | [] -> event_loop current_tool
          | st :: rest ->
              undo_stack := rest;
              redo_stack := current_state () :: !redo_stack;
              restore_state st;
              let new_img = make_display_image () in
              redraw new_img !img_x_ref !img_y_ref !w_ref !h_ref toolbar_x ""
                !brightness_level;
              event_loop current_tool)
        else if key = 'y' then (
          match !redo_stack with
          | [] -> event_loop current_tool
          | st :: rest ->
              redo_stack := rest;
              undo_stack := current_state () :: !undo_stack;
              restore_state st;
              let new_img = make_display_image () in
              redraw new_img !img_x_ref !img_y_ref !w_ref !h_ref toolbar_x ""
                !brightness_level;
              event_loop current_tool)
        else if
          key = 'c' && current_tool = "cut" && List.length !clicked_points >= 2
        then (
          if List.length !clicked_points = 2 then (
            let pts = List.rev !clicked_points in
            let p1 = List.nth pts 0 in
            let p2 = List.nth pts 1 in
            let cut_data = cut_square !img_data p1 p2 in
            prev_cut := array_sub !img_data cut_data;
            push_undo ();
            img_data := cut_data;
            add_message "Applying square cut.")
          else begin
            let cut_data = cut !img_data (List.rev !clicked_points) in
            prev_cut := array_sub !img_data cut_data;
            push_undo ();
            img_data := cut_data;
            List.iter
              (fun (x, y) ->
                add_message
                  ("(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"))
              (List.rev !clicked_points);
            add_message "Cut applied with points:";
            flush stdout
          end;
          let new_img = make_display_image () in
          redraw new_img !img_x_ref !img_y_ref !w_ref !h_ref toolbar_x
            current_tool !brightness_level;

          clicked_points := [];
          add_message "Points reset. Select a tool and continue.";
          event_loop "")
        else if
          key = 'f' && current_tool = "fill" && List.length !clicked_points >= 2
        then (
          if List.length !clicked_points = 2 then (
            let pts = List.rev !clicked_points in
            let p1 = List.nth pts 0 in
            let p2 = List.nth pts 1 in
            let fill_data = fill_square !img_data p1 p2 !fill_color in
            prev_cut := array_sub !img_data fill_data;
            push_undo ();
            img_data := fill_data;
            add_message "Applying square fill.\n")
          else begin
            let fill_data =
              fill !img_data (List.rev !clicked_points) !fill_color
            in
            prev_cut := array_sub !img_data fill_data;
            push_undo ();
            img_data := fill_data;
            add_message "Fill applied with points:\n";
            List.iter
              (fun (x, y) ->
                add_message
                  ("(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"))
              (List.rev !clicked_points);
            flush stdout
          end;
          let new_img = make_display_image () in
          redraw new_img !img_x_ref !img_y_ref !w_ref !h_ref toolbar_x
            current_tool !brightness_level;

          fill_color := Graphics.rgb 0 0 0;
          clicked_points := [];
          add_message "Points reset. Select a tool and continue.";
          event_loop "")
        else if
          key = 'p' && current_tool = "paste" && List.length !clicked_points = 1
        then (
          (* Currenlty assumes pasting only a square. *)
          let paste_point = List.hd !clicked_points in
          let pasted_data =
            paste !prev_cut !img_data
              (array_plus !prev_cut !img_data)
              paste_point
          in
          push_undo ();
          img_data := pasted_data;
          add_message
            (Printf.sprintf "Paste applied at point: (%d, %d)" (fst paste_point)
               (snd paste_point));
          let new_img = make_display_image () in
          redraw new_img !img_x_ref !img_y_ref !w_ref !h_ref toolbar_x
            current_tool !brightness_level;

          clicked_points := [];
          add_message "Points reset. Select a tool and continue.";
          event_loop "" (* point reset (tool command) *))
        else if key = 'r' then (
          clicked_points := [];
          add_message "Points reset.";
          event_loop current_tool (* Save: 's' *))
        else if key = 's' then (
          add_message "Save (keyboard): Enter filename (without extension): ";
          let filename = read_line () in
          add_message "Saving as PNG...";

          let pixels_to_save =
            if !brightness_level = 0 then !img_data
            else adjust_brightness !img_data !brightness_level
          in
          FinalProject.FileSaver.save_image_to_png pixels_to_save
            (filename ^ ".png");

          add_message (Printf.sprintf "Image saved as %s.png" filename);
          event_loop "save"
          (* Zoom in: '+' *))
        else if key = '=' || key = '+' then
          if !zoom_level >= 3 then event_loop current_tool
          else (
            if !zoom_level = 0 then zoom_base := Array.map Array.copy !img_data;

            zoom_level := !zoom_level + 1;
            push_undo ();
            img_data := apply_zoom !zoom_level !zoom_base;

            let new_h = Array.length !img_data in
            let new_w = if new_h = 0 then 0 else Array.length !img_data.(0) in
            h_ref := new_h;
            w_ref := new_w;

            let win_w = size_x () in
            let win_h = size_y () in
            img_x_ref := (toolbar_x - !w_ref) / 2;
            img_y_ref :=
              message_panel_h + ((win_h - message_panel_h - !h_ref) / 2);
            let new_img = make_display_image () in
            clear_graph ();
            draw_image new_img !img_x_ref !img_y_ref;
            draw_axes !img_x_ref !img_y_ref !w_ref !h_ref;
            draw_toolbar win_w win_h toolbar_x "zoom_in" !brightness_level;
            draw_message_panel win_w message_panel_h;
            synchronize ();
            event_loop "zoom_in"
            (* Zoom out: '-' *))
        else if key = '-' then
          if !zoom_level <= -3 then event_loop current_tool
          else (
            if !zoom_level = 0 then zoom_base := Array.map Array.copy !img_data;

            zoom_level := !zoom_level - 1;
            push_undo ();
            img_data := apply_zoom !zoom_level !zoom_base;

            let new_h = Array.length !img_data in
            let new_w = if new_h = 0 then 0 else Array.length !img_data.(0) in
            h_ref := new_h;
            w_ref := new_w;

            let win_w = size_x () in
            let win_h = size_y () in
            img_x_ref := (toolbar_x - !w_ref) / 2;
            img_y_ref :=
              message_panel_h + ((win_h - message_panel_h - !h_ref) / 2);
            let new_img = make_display_image () in
            clear_graph ();
            draw_image new_img !img_x_ref !img_y_ref;
            draw_axes !img_x_ref !img_y_ref !w_ref !h_ref;
            draw_toolbar win_w win_h toolbar_x "zoom_out" !brightness_level;
            draw_message_panel win_w message_panel_h;
            synchronize ();
            event_loop "zoom_out"
            (* Full reset: 'R' *))
        else if key = 'R' then (
          push_undo ();
          img_data := Array.map Array.copy original_data;
          w_ref := original_w;
          h_ref := original_h;
          img_x_ref := original_img_x;
          img_y_ref := original_img_y;
          zoom_level := 0;
          zoom_base := Array.map Array.copy !img_data;
          pixel_fact := 2;

          let win_w = size_x () in
          let win_h = size_y () in
          brightness_level := 0;
          let new_img = make_display_image () in
          clear_graph ();
          draw_image new_img !img_x_ref !img_y_ref;
          draw_axes !img_x_ref !img_y_ref !w_ref !h_ref;
          draw_toolbar win_w win_h toolbar_x "reset" !brightness_level;
          draw_message_panel win_w message_panel_h;
          add_message "Image reset to original (keyboard).";
          synchronize ();
          event_loop "reset")
        else if key = 'q' then (
          add_message "Exiting.";
          exit 0)
        else event_loop current_tool
      else (
        Unix.sleepf 0.01;
        event_loop current_tool)
    with exn -> (
      match exn with
      | Graphics.Graphic_failure msg ->
          prerr_endline ("Graphics closed in event_loop: " ^ msg);
          exit 0
      | _ ->
          log_exception "event_loop" exn;
          Unix.sleepf 0.2;
          event_loop current_tool)
  in
  event_loop ""

(** [handle_click img_x img_y w h] waits for mouse clicks and prints the
    image-local coordinates of clicked pixels *)
let handle_click img_x img_y w h =
  let rec click_loop () =
    try
      if button_down () then (
        let screen_x, screen_y = mouse_pos () in
        let img_px, img_py =
          screen_to_image_coords screen_x screen_y img_x img_y
        in
        if is_within_bounds img_px img_py w h then
          add_message
            (Printf.sprintf "Clicked at image coordinates: (%d, %d)" img_px
               img_py)
        else add_message "Click outside image bounds";
        Unix.sleepf 0.2;
        click_loop ())
      else if key_pressed () then ignore (read_key ())
      else (
        Unix.sleepf 0.01;
        click_loop ())
    with exn -> (
      match exn with
      | Graphics.Graphic_failure msg ->
          prerr_endline ("Graphics closed in click_loop: " ^ msg);
          exit 0
      | _ ->
          log_exception "click_loop" exn;
          Unix.sleepf 0.2;
          click_loop ())
  in
  click_loop ()

let () =
  try
    if Array.length Sys.argv < 2 then usage ();
    let path = Sys.argv.(1) in

    if not (Sys.file_exists path) then (
      Printf.printf "File not found: %s\n" path;
      exit 1);

    open_graph " 1000x700";
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
    let img_y = message_panel_h + ((win_h - message_panel_h - h) / 2) in
    draw_image img img_x img_y;

    (* Convert image to pixel array for cut operations *)
    let data =
      try ref (Graphics.dump_image img)
      with e ->
        log_exception "dump_image" e;
        (* Fall back to a 0x0 image to avoid hard crash. *)
        ref (Array.make_matrix 0 0 0)
    in

    draw_axes img_x img_y w h;
    draw_toolbar win_w win_h toolbar_x "" 0;
    synchronize ();
    add_message "Welcome to CamlShop!";
    add_message
      "Keyboard: u = undo, y = redo, q = quit, +/- = zoom, s = save, R = reset.";

    handle_buttons img_x img_y w h data toolbar_x
  with exn -> (
    match exn with
    | Graphics.Graphic_failure msg ->
        prerr_endline ("Graphics closed in main: " ^ msg);
        exit 0
    | _ ->
        prerr_endline ("Fatal error in main: " ^ Printexc.to_string exn);
        Printexc.print_backtrace stderr;
        (try add_message "Fatal error occurred. Exiting CamlShop."
         with _ -> ());
        exit 1)
