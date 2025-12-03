val load_image_raw : string -> Images.t
(** [load_image_raw path] loads the image at [path] and returns the raw Images.t
    variant. No graphics window needed. *)

val image_dimensions : Images.t -> int * int
(** [image_dimensions img] returns the width and height of the image, regardless
    of format. *)

val make_graphics_image : Images.t -> Graphics.image
(** [make_graphics_image img] converts a raw Images.t to a Graphics.image.
    Requires graphics window. *)

val graphics_image_of_file : string -> Graphics.image * int * int
(** [graphics_image_of_file path] loads the image at [path] and returns a tuple
    of a [Graphics.image] and its width and height (in pixels). The function
    handles several on-disk formats by converting them to the rgb24 format
    expected by the Graphics library. *)
