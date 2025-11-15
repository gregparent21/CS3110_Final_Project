(** Image coordinate and pixel operations for the image viewer. *)

(** [screen_to_image_coords screen_x screen_y img_x img_y] converts a screen 
    position [(screen_x, screen_y)] to image-local coordinates, given that the 
    image is positioned at [(img_x, img_y)] in screen space. Returns a tuple 
    [(image_x, image_y)]. *)
val screen_to_image_coords : int -> int -> int -> int -> int * int

(** [is_within_bounds x y w h] checks if pixel [(x, y)] is within the bounds of 
    an image of width [w] and height [h]. *)
val is_within_bounds : int -> int -> int -> int -> bool

(** [pixel_to_string r g b] formats an RGB pixel as a readable string. *)
val pixel_to_string : int -> int -> int -> string
