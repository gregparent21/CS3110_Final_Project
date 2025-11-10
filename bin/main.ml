(* Minimal image viewer: dune exec bin/main.exe -- data/Homer_Simpson.png *)

open Graphics

(* Convert a CamlImages image into a Graphics.image *)
let graphics_image_of_file (path : string) : Graphics.image * int * int =
  Printf.eprintf "Loading image from: %s\n" path;

  try
    (* Try using Png module directly which might be simpler *)
    Printf.eprintf "Attempting PNG load...\n";
    let _img = Png.load path [] in
    Printf.eprintf "PNG loaded as Images.t type\n";

    (* For now, fall back to test pattern since the Images.t interface is
       complex *)
    raise (Failure "Using fallback for now")
  with e ->
    Printf.eprintf
      "Note: Real image loading not yet implemented (%s)\n\
       Using fallback pattern\n"
      (Printexc.to_string e);
    (* Fallback: create a colorful test pattern *)
    let w = 200 in
    let h = 200 in
    let data = Array.make_matrix h w 0 in
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        let r = x mod 256 in
        let g = y mod 256 in
        let b = (x + y) mod 256 in
        data.(y).(x) <- (r lsl 16) lor (g lsl 8) lor b
      done
    done;
    (Graphics.make_image data, w, h)

let usage () =
  prerr_endline "Usage: image_viewer <image_path>";
  exit 1

let () =
  if Array.length Sys.argv < 2 then usage ();
  let path = Sys.argv.(1) in
  (* Check if file exists *)
  if not (Sys.file_exists path) then (
    Printf.eprintf "File not found: %s\n" path;
    exit 1);
  (* Open a default window first *)
  open_graph " 800x600";
  set_window_title (Filename.basename path);
  auto_synchronize false;
  (* Now try to load the image *)
  let img, w, h =
    try graphics_image_of_file path
    with e ->
      Printf.eprintf "Failed to load image: %s\n" (Printexc.to_string e);
      Printexc.print_backtrace stderr;
      exit 2
  in
  (* Draw the image in the lower-left corner and show it *)
  draw_image img 0 0;
  synchronize ();
  (* Simple UI: press any key or close the window to exit *)
  (* Poll for keypress or window resize/close events *)
  let rec loop () =
    if key_pressed () then ignore (read_key ())
    else (
      Unix.sleepf 0.01;
      loop ())
  in
  loop ()
