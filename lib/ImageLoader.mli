(** Image loading and conversion helpers. *)

val graphics_image_of_file : string -> Graphics.image * int * int
(** [graphics_image_of_file path] loads the image at [path] and returns a tuple
    of a [Graphics.image] and its width and height (in pixels). The function
    handles several on-disk formats by converting them to the rgb24 format
    expected by the Graphics library. *)
