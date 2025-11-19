let screen_to_image_coords screen_x screen_y img_x img_y =
  (screen_x - img_x, screen_y - img_y)

let is_within_bounds x y w h = x >= 0 && x < w && y >= 0 && y < h
let pixel_to_string r g b = Printf.sprintf "RGB(%d, %d, %d)" r g b

let standard_coordinates ((a, b) : int * int) ((x, y) : int * int) =
  if a < x && b > y then ((a, y), (x, b))
  else if a > x && b < y then ((x, b), (a, y))
  else if a > x && b > y then ((x, y), (a, b))
  else ((a, b), (x, y))

let cut_square (data : int array array) ((start_x, start_y) : int * int)
    ((end_x, end_y) : int * int) =
  try
    let (a, b), (u, v) =
      standard_coordinates (start_x, start_y) (end_x, end_y)
    in
    let height = Array.length data in
    Array.mapi
      (fun temp row ->
        let y = height - temp - 1 in
        Array.mapi
          (fun x rgb ->
            if a <= x && x <= u && b <= y && y <= v then
              Graphics.rgb 255 255 255
            else rgb)
          row)
      data
  with _ -> raise (Failure "cut_advanced: invalid coordinates")

let on_segment ((s_x, s_y) : int * int) ((e_x, e_y) : int * int)
    ((p_x, p_y) : int * int) =
  ((e_x - s_x) * (p_y - s_y)) - ((e_y - s_y) * (p_x - s_x)) = 0
  && p_x >= min s_x e_x
  && p_x <= max s_x e_x
  && p_y >= min s_y e_y
  && p_y <= max s_y e_y

(* Determines if the ray in the positive x infinity direction from (x, y)
   intersects the line segment from (x1, y1) to (x2, y2).*)
let intersects_segment ((x, y) : int * int) ((x1, y1) : int * int)
    ((x2, y2) : int * int) =
  (* Ensure y1 <= y2 *)
  let x1, y1, x2, y2 =
    if y1 <= y2 then (x1, y1, x2, y2) else (x2, y2, x1, y1)
  in
  (* Check if horizontal ray crosses the segment *)
  y > y1 && y <= y2
  && float x
     <= (float x2 -. float x1)
        *. (float y -. float y1)
        /. (float y2 -. float y1)
        +. float x1

(* Currently supports only SIMPLE polygons. *)
let cut_advanced (data : int array array) (pairs : (int * int) list) =
  let n = List.length pairs in
  if n < 3 then
    raise (Failure "cut_advanced: polygon must have at least 3 vertices");
  try
    let segments =
      List.mapi
        (fun i pair -> (pair, List.nth pairs ((i + 1) mod List.length pairs)))
        pairs
    in
    let height = Array.length data in
    Array.mapi
      (fun temp row ->
        let y = height - temp - 1 in
        Array.mapi
          (fun x rgb ->
            let crossings =
              List.fold_left
                (fun acc (s, e) ->
                  if intersects_segment (x, y) s e then acc + 1 else acc)
                0 segments
            in
            let on =
              List.exists (fun (px, py) -> on_segment px py (x, y)) segments
            in
            if crossings mod 2 = 1 || List.mem (x, y) pairs || on then
              Graphics.rgb 255 255 255
            else rgb)
          row)
      data
  with _ -> raise (Failure "cut_advanced: invalid coordinates")

let paste (base : int array array) (overlay : int array array)
    ((x, y) : int * int) : int array array =
  let overlay_height = Array.length overlay in
  let overlay_width = Array.length overlay.(0) in
  let base_height = Array.length base in
  let y = base_height - y - 1 in
  Array.mapi
    (fun temp row ->
      let i = temp - 1 - base_height in
      Array.mapi
        (fun j pixel ->
          if i >= y && i < y + overlay_height && j >= x && j < x + overlay_width
          then overlay.(i - y).(j - x)
          else pixel)
        row)
    base

(**[r] takes a pixel point and returns that pixel's red value (0-255).*)
let r p = (p lsr 16) land 0xFF

(**[g] takes a pixel point and returns that pixel's red value (0-255).*)
let g p = (p lsr 8) land 0xFF

(**[b] takes a pixel point and returns that pixel's red value (0-255).*)
let b p = p land 0xFF

(**[average_neighbors] takes a data matrix of RGB values and averages squares of
   pixels of size (factor * factor) into 1 averaged pixel. Returns the averaged
   RGB value.*)
let average_neighbors (data : int array array) (factor : int) (x : int)
    (y : int) =
  try
    let avg_r = ref 0 in
    let avg_g = ref 0 in
    let avg_b = ref 0 in
    for i = 0 to factor - 1 do
      for j = 0 to factor - 1 do
        let p = data.(x + i).(y + j) in
        avg_r := !avg_r + r p;
        avg_g := !avg_g + g p;
        avg_b := !avg_b + b p
      done
    done;
    avg_r := !avg_r / (factor * factor);
    avg_g := !avg_g / (factor * factor);
    avg_b := !avg_b / (factor * factor);
    (!avg_r lsl 16) lor (!avg_g lsl 8) lor !avg_b
  with _ -> failwith "Out of bounds"

let shrink (data : int array array) (factor : int) : int array array =
  if factor > 0 then
    try
      let data' =
        Array.make_matrix
          (Array.length data / factor)
          (Array.length data.(0) / factor)
          0
      in
      for x = 0 to (Array.length data / factor) - 1 do
        for y = 0 to (Array.length data.(0) / factor) - 1 do
          data'.(x).(y) <-
            average_neighbors data factor (x * factor) (y * factor)
        done
      done;
      data'
    with _ -> failwith "Out of bounds"
  else failwith "Invalid input"

let replace_color (data : int array array)
    ((src_r, src_g, src_b) : int * int * int)
    ((dst_r, dst_g, dst_b) : int * int * int) : int array array =
  Array.map
    (fun row ->
      Array.map
        (fun pixel ->
          if r pixel = src_r && g pixel = src_g && b pixel = src_b then
            Graphics.rgb dst_r dst_g dst_b
          else pixel)
        row)
    data

let invert_colors (data : int array array) : int array array =
  Array.map
    (fun row ->
      Array.map
        (fun p ->
          let ir = 255 - r p in
          let ig = 255 - g p in
          let ib = 255 - b p in
          Graphics.rgb ir ig ib)
        row)
    data
