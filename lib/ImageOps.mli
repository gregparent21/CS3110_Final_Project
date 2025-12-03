(** Image coordinate and pixel operations for the image viewer. *)

val screen_to_image_coords : int -> int -> int -> int -> int * int
(** [screen_to_image_coords screen_x screen_y img_x img_y] converts a screen
    position [(screen_x, screen_y)] to image-local coordinates, given that the
    image is positioned at [(img_x, img_y)] in screen space. Returns a tuple
    [(image_x, image_y)]. *)

val is_within_bounds : int -> int -> int -> int -> bool
(** [is_within_bounds x y w h] checks if pixel [(x, y)] is within the bounds of
    an image of width [w] and height [h]. *)

val pixel_to_string : int -> int -> int -> string
(** [pixel_to_string r g b] formats an RGB pixel as a readable string. *)

val cut_square : int array array -> int * int -> int * int -> int array array
(** [cut_square img x y] removes the square with corners x and y from img. *)

val cut : int array array -> (int * int) list -> int array array
(** [cut_advanced img lst] removes the polygon defined by the line segments
    between each two conseuctive pairs of integers (wrapping the last pair to
    the first). *)

val fill_square :
  int array array -> int * int -> int * int -> int -> int array array
(** [fill_square img x y color] fills the square with corners x and y in img
    with color. *)

val fill : int array array -> (int * int) list -> int -> int array array
(** [fill img lst color] fills the polygon defined by the line segments between
    each two conseuctive pairs of integers (wrapping the last pair to the first)
    with color. *)

val paste : int array array -> int array array -> int * int -> int array array
(** [paste base overlay (x, y)] pastes the [overlay] image onto the [base] image
    with the bottom-left corner of the overlay at position [(x, y)] in the base
    image. *)

val shrink : int array array -> int array array
(**[shrink] "compresses" a given image by reducing its total pixel count and
   averaging the RGB values of neighboring pixels. Reduces the size of an image
   by a factor of 2.*)

val enlarge : int array array -> int array array
(**[enlarge] enlarges a given image by a factor of 2 in each dimension using
   nearest-neighbor.*)

val replace_color :
  int array array -> int * int * int -> int * int * int -> int array array
(** [replace_color data (src_r, src_g, src_b) (dst_r, dst_g, dst_b)] returns a
    new pixel matrix where every pixel whose RGB components exactly match
    [(src_r, src_g, src_b)] is replaced with [(dst_r, dst_g, dst_b)]. *)

val invert_colors : int array array -> int array array
(** [invert_colors data] returns a new pixel matrix where each pixel's RGB
    values are inverted: (r, g, b) becomes (255 - r, 255 - g, 255 - b). *)

val flip_horizontal : int array array -> int array array
(** [flip_horizontal img] returns a new image that is a horizontal mirror of
    [img]. *)

val crop : int array array -> int * int -> int * int -> int array array
(** [crop data (x1, y1) (x2, y2)] returns a new pixel matrix consisting only of
    the rectangular region between [(x1, y1)] and [(x2, y2)], inclusive. *)

val array_sub : int array array -> int array array -> int array array
(** [array_sub A B] returns A-B elementwise. Requires that [A] and [B] are the
    same size. *)

val pixelate : int array array -> int -> int array array
(**[pixelate] returns a new image that is a pixelated version of the original
   image. It pixelates the image based on a pixelation factor. As an example, a
   pixelation factor of 2 will average out the RGB value of a 2x2 area and set
   all pixels in that area to that RGB value. This will repeat throughout the
   entire image.*)
