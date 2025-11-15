let screen_to_image_coords screen_x screen_y img_x img_y =
  (screen_x - img_x, screen_y - img_y)

let is_within_bounds x y w h =
  x >= 0 && x < w && y >= 0 && y < h

let pixel_to_string r g b =
  Printf.sprintf "RGB(%d, %d, %d)" r g b
