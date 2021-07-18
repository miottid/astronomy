type dms = { degrees: float; minutes: float; seconds: float }

val pp_dms : dms -> string
(** [pp_dms dms] converts degrees, minutes and seconds to an human readable string. *)

val degrees_of_dms : dms -> float
(** [degrees_of_dms] converts degrees, minutes and seconds to decimal degrees. *)

val dms_of_degrees : float -> dms
(** [dms_of_degrees degrees] converts decimal degrees to degrees, minutes and seconds. *)
