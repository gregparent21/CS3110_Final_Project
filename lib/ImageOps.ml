let screen_to_image_coords screen_x screen_y img_x img_y =
  (screen_x - img_x, screen_y - img_y)

let is_within_bounds x y w h = x >= 0 && x < w && y >= 0 && y < h
let pixel_to_string r g b = Printf.sprintf "RGB(%d, %d, %d)" r g b

let standard_coordinates ((a, b) : int * int) ((x, y) : int * int) =
  if a < x && b > y then ((a, y), (x, b))
  else if a > x && b < y then ((x, b), (a, y))
  else if a > x && b > y then ((x, y), (a, b))
  else ((a, b), (x, y))

(* Inline testing for standard coordinates. *)
let%test "standard_coordinates" =
  (standard_coordinates (1, 4) (4, 1) = ((1, 1), (4, 4))) [@coverage off]

let%test "standard_coordinates 2" =
  (standard_coordinates (4, 1) (1, 4) = ((1, 1), (4, 4))) [@coverage off]

let%test "standard_coordinates 3" =
  (standard_coordinates (1, 1) (4, 4) = ((1, 1), (4, 4))) [@coverage off]

let%test "standard_coordinates 4" =
  (standard_coordinates (4, 4) (1, 1) = ((1, 1), (4, 4))) [@coverage off]

(* Determines if the point p is on the line segment from s to e. *)
let on_segment ((s_x, s_y) : int * int) ((e_x, e_y) : int * int)
    ((p_x, p_y) : int * int) =
  ((e_x - s_x) * (p_y - s_y)) - ((e_y - s_y) * (p_x - s_x)) = 0
  && p_x >= min s_x e_x
  && p_x <= max s_x e_x
  && p_y >= min s_y e_y
  && p_y <= max s_y e_y

(* Inline testing for on segment. *)
let%test "on_segment" = on_segment (0, 0) (10, 10) (5, 5) [@coverage off]
let%test "on_segment 2" = on_segment (0, 10) (10, 0) (5, 5) [@coverage off]
let%test "on_segment 3" = on_segment (1, 1) (2, 2) (1, 1) [@coverage off]
let%test "on_segment 4" = on_segment (1, 1) (2, 2) (2, 2) [@coverage off]

let%test "on_segment 5" =
  not (on_segment (0, 0) (10, 10) (5, 6)) [@coverage off]

let%test "on_segment 6" =
  not (on_segment (0, 10) (10, 0) (6, 5)) [@coverage off]

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

(* Inline testing for intersects segment. *)
let%test "intersects_segment" =
  intersects_segment (5, 5) (0, 0) (10, 10) [@coverage off]

let%test "intersects_segment 2" =
  intersects_segment (0, 5) (0, 10) (10, 0) [@coverage off]

let%test "intersects_segment 3" =
  intersects_segment (0, 10) (0, 10) (10, 0) [@coverage off]

let%test "intersects_segment 4" =
  not (intersects_segment (15, 15) (0, 0) (10, 10)) [@coverage off]

let%test "intersects_segment 5" =
  not (intersects_segment (5, 15) (0, 0) (10, 10)) [@coverage off]

let%test "intersects_segment 6" =
  not (intersects_segment (10, 5) (0, 0) (10, 10)) [@coverage off]

let fill_square (data : int array array) ((start_x, start_y) : int * int)
    ((end_x, end_y) : int * int) (color : int) =
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
            if a <= x && x <= u && b <= y && y <= v then color else rgb)
          row)
      data
  with _ -> raise (Failure "invalid coordinates")

let fill (data : int array array) (pairs : (int * int) list) (color : int) =
  let n = List.length pairs in
  if n < 3 then raise (Failure "polygon must have at least 3 vertices");
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
            if crossings mod 2 = 1 || List.mem (x, y) pairs || on then color
            else rgb)
          row)
      data
  with _ -> raise (Failure "invalid coordinates")

let cut_square (data : int array array) ((start_x, start_y) : int * int)
    ((end_x, end_y) : int * int) =
  fill_square data (start_x, start_y) (end_x, end_y) (Graphics.rgb 255 255 255)

let cut (data : int array array) (pairs : (int * int) list) =
  fill data pairs (Graphics.rgb 255 255 255)

let paste (data : int array array) (image : int array array)
    ((start_x, start_y) : int * int) =
  let image_height = Array.length image in
  let image_width = Array.length image.(0) in
  let data_height = Array.length data in
  let data_width = Array.length data.(0) in
  if
    start_x < 0 || start_y < 0
    || start_x + image_width > data_width
    || start_y + image_height > data_height
  then raise (Failure "Paste operation out of bounds");
  for i = 0 to image_height - 1 do
    for j = 0 to image_width - 1 do
      data.(start_y + i).(start_x + j) <- image.(i).(j)
    done
  done;
  data

(**[r] takes a pixel point and returns that pixel's red value (0-255).*)
let r p = (p lsr 16) land 0xFF

let%test "r gets the red component" =
  (let p = Graphics.rgb 123 45 67 in
   r p = 123)
  [@coverage off]

(**[g] takes a pixel point and returns that pixel's red value (0-255).*)
let g p = (p lsr 8) land 0xFF

let%test "g gets the green component" =
  (let p = Graphics.rgb 123 45 67 in
   g p = 45)
  [@coverage off]

(**[b] takes a pixel point and returns that pixel's red value (0-255).*)
let b p = p land 0xFF

let%test "b gets the blue component" =
  (let p = Graphics.rgb 123 45 67 in
   b p = 67)
  [@coverage off]

(**[average_neighbors] takes a data matrix of RGB values and averages squares of
   pixels of size (factor * factor) into 1 averaged pixel. Returns the averaged
   RGB value.*)
let average_neighbors (data : int array array) (factor : int) (x : int)
    (y : int) =
  let height = Array.length data in
  let width = Array.length data.(0) in
  if x >= height || x < 0 || y >= width || y < 0 then failwith "Out of bounds"
  else
    let x_end = min (x + factor) height in
    let y_end = min (y + factor) width in
    let avg_r = ref 0 in
    let avg_g = ref 0 in
    let avg_b = ref 0 in
    for i = x to x_end - 1 do
      for j = y to y_end - 1 do
        let p = data.(i).(j) in
        avg_r := !avg_r + r p;
        avg_g := !avg_g + g p;
        avg_b := !avg_b + b p
      done
    done;
    avg_r := !avg_r / (factor * factor);
    avg_g := !avg_g / (factor * factor);
    avg_b := !avg_b / (factor * factor);
    (!avg_r lsl 16) lor (!avg_g lsl 8) lor !avg_b

let%test "average_neighbors on a 1x1 image" =
  let p = Graphics.rgb 50 60 70 in
  let data = [| [| p |] |] in
  (average_neighbors data 1 0 0 = p) [@coverage off]

let%test "average_neighbors on a 2x2 image" =
  let a = Graphics.rgb 10 20 30 in
  let b = Graphics.rgb 20 30 40 in
  let c = Graphics.rgb 30 40 50 in
  let d = Graphics.rgb 40 50 60 in
  let data = [| [| a; b |]; [| c; d |] |] in
  let avg = Graphics.rgb 25 35 45 in
  (average_neighbors data 2 0 0 = avg) [@coverage off]

let shrink (data : int array array) : int array array =
  let data' =
    Array.make_matrix (Array.length data / 2) (Array.length data.(0) / 2) 0
  in
  for x = 0 to (Array.length data / 2) - 1 do
    for y = 0 to (Array.length data.(0) / 2) - 1 do
      data'.(x).(y) <- average_neighbors data 2 (x * 2) (y * 2)
    done
  done;
  data'

let enlarge (data : int array array) : int array array =
  let h = Array.length data in
  if h = 0 then [||]
  else
    let w = Array.length data.(0) in
    let h' = h * 2 in
    let w' = w * 2 in
    let data' = Array.make_matrix h' w' 0 in
    for x = 0 to h - 1 do
      for y = 0 to w - 1 do
        let p = data.(x).(y) in
        data'.(x * 2).(y * 2) <- p;
        data'.((x * 2) + 1).(y * 2) <- p;
        data'.(x * 2).((y * 2) + 1) <- p;
        data'.((x * 2) + 1).((y * 2) + 1) <- p
      done
    done;
    data'

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

let grayscale (data : int array array) : int array array =
  Array.map
    (fun row ->
      Array.map
        (fun p ->
          let r = r p in
          let g = g p in
          let b = b p in
          (* integer approximation of 0.3R + 0.59G + 0.11B *)
          let y = ((30 * r) + (59 * g) + (11 * b)) / 100 in
          Graphics.rgb y y y)
        row)
    data

let flip_horizontal (img : int array array) : int array array =
  let height = Array.length img in
  if height = 0 then img
  else
    let width = Array.length img.(0) in
    Array.init height (fun y ->
        Array.init width (fun x -> img.(y).(width - 1 - x)))

let crop (data : int array array) ((x1, y1) : int * int) ((x2, y2) : int * int)
    : int array array =
  let height = Array.length data in
  if height = 0 then [||]
  else
    let width = Array.length data.(0) in

    (* map "geometry" y (0 = bottom) to array row index (0 = top) *)
    let geom_y_to_row y = height - 1 - y in

    (* clamp to valid geometry range before converting *)
    let clamp v lo hi = max lo (min v hi) in

    let gx1 = clamp x1 0 (width - 1) in
    let gx2 = clamp x2 0 (width - 1) in
    let gy1 = clamp y1 0 (height - 1) in
    let gy2 = clamp y2 0 (height - 1) in

    let r1 = geom_y_to_row gy1 in
    let r2 = geom_y_to_row gy2 in

    (* normalize to min/max in array index space *)
    let x_min = min gx1 gx2 in
    let x_max = max gx1 gx2 in
    let y_min = min r1 r2 in
    let y_max = max r1 r2 in

    let new_h = y_max - y_min + 1 in
    let new_w = x_max - x_min + 1 in

    let result = Array.make_matrix new_h new_w 0 in
    for j = 0 to new_h - 1 do
      for i = 0 to new_w - 1 do
        result.(j).(i) <- data.(y_min + j).(x_min + i)
      done
    done;
    result

let array_sub (a : int array array) (b : int array array) : int array array =
  try
    Array.mapi
      (fun j row -> Array.mapi (fun i pixel -> pixel - b.(j).(i)) row)
      a
  with _ -> raise (Failure "Array Subtraction Error!")

(**[square_replace] is a helper function for the pixelate function. Takes in the
   reference to the new image created in pixelate and does all the pixelation
   where we replace square segments of the image with the same RGB value.
   `bounds` determines the dimensions of the square segment.*)
let square_replace (data : int array array) (bounds : int) (x : int) (y : int)
    (avg : int) : unit =
  let height = Array.length data in
  let width = Array.length data.(0) in
  let x_end = min (x + bounds) height in
  let y_end = min (y + bounds) width in
  for x = x to x_end - 1 do
    for y = y to y_end - 1 do
      data.(x).(y) <- avg
    done
  done

let pixelate (data : int array array) (factor : int) : int array array =
  if factor > 0 && Array.length data > 0 then (
    let height = Array.length data in
    let width = Array.length data.(0) in
    let blocks_x = (height + factor - 1) / factor in
    let blocks_y = (width + factor - 1) / factor in
    for bx = 0 to blocks_x - 1 do
      for by = 0 to blocks_y - 1 do
        let start_x = bx * factor in
        let start_y = by * factor in
        let avg = average_neighbors data factor start_x start_y in
        square_replace data factor start_x start_y avg
      done
    done;
    data)
  else data
