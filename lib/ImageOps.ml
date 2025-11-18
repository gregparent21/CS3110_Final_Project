let screen_to_image_coords screen_x screen_y img_x img_y =
  (screen_x - img_x, screen_y - img_y)

let is_within_bounds x y w h = x >= 0 && x < w && y >= 0 && y < h
let pixel_to_string r g b = Printf.sprintf "RGB(%d, %d, %d)" r g b

let cut_square data (start_x, start_y) (end_x, end_y) =
  try
    Array.mapi
      (fun y row ->
        Array.mapi
          (fun x rgb ->
            if start_x <= x && x <= end_x && start_y <= y && y <= end_y then
              Graphics.rgb 255 255 255
            else rgb)
          row)
      data
  with _ -> raise (Failure "cut_advanced: invalid coordinates")

(** [left (s_x, s_y) (e_x, e_y) (p_x, p_y)] determines if the point (p_x, p_y)
    lives to the left or right of the directed line segment. *)
let left (start_x, start_y) (end_x, end_y) (point_x, point_y) =
  ((end_x - start_x) * (point_y - start_y))
  - ((end_y - start_y) * (point_x - start_x))
  > 0

let on_segment (s_x, s_y) (e_x, e_y) (p_x, p_y) =
  ((e_x - s_x) * (p_y - s_y)) - ((e_y - s_y) * (p_x - s_x)) = 0
  && p_x >= min s_x e_x
  && p_x <= max s_x e_x
  && p_y >= min s_y e_y
  && p_y <= max s_y e_y

(** [winding_number (p_x, p_y) polygon] determines the winding number of the
    point (p_x, p_y) with respect to the possible non-simple polygon. A zero
    winding number means the point lies in the exterior and a non-zero winding
    number means the point lies in the interior. *)
let winding_number (p_x, p_y) polygon =
  let n = List.length polygon in
  let wn = ref 0 in
  for i = 0 to n - 1 do
    let x0, y0 = List.nth polygon i in
    let x1, y1 = List.nth polygon ((i + 1) mod n) in
    if on_segment (x0, y0) (x1, y1) (p_x, p_y) then wn := 1
    else if y0 <= p_y then
      if y1 > p_y && left (x0, y0) (x1, y1) (p_x, p_y) then wn := !wn + 1
      else if y1 <= p_y && not (left (x0, y0) (x1, y1) (p_x, p_y)) then
        wn := !wn - 1
  done;
  !wn

(* Currently uses an inefficient algorithm (checks every possible pixel) that
   handles even complex polygons. The algorithm is much simpler if we care about
   only simple polygons. *)
let cut_advanced data pairs =
  try
    Array.mapi
      (fun y row ->
        Array.mapi
          (fun x rgb ->
            if winding_number (x, y) pairs <> 0 then Graphics.rgb 255 255 255
            else rgb)
          row)
      data
  with _ -> raise (Failure "cut_advanced: invalid coordinates")

let shrink data =
  try
  let data' = Array.make ((Array.length data) / 3) (Array.make (Array.length data.(0)) / 3 )