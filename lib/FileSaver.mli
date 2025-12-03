(* File saving helpers for edited images. *)

val save_image_to_png : int array array -> string -> unit
(** [save_image_to_png data output_path] saves the pixel data [data] (as an
    RGB24 array) to a PNG file at [output_path]. Raises an exception if the file
    cannot be written. *)
(* JPEG saving removed: no JPEG API exposed. *)
