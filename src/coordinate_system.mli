type dms = { degrees: float; minutes: float; seconds: float }

val pp_dms : dms -> string
(** [pp_dms dms] converts degrees, minutes and seconds to an human readable string. *)

val dms_of_degrees : float -> dms
(** [dms_of_degrees degress] converts decimal degress to degress, minutes and seconds. *)