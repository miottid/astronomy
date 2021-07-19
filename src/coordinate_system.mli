type dms = { degrees: float; minutes: float; seconds: float }

val pp_dms : dms -> string
(** [pp_dms dms] converts degrees, minutes and seconds to 
    an human readable string. *)

val deg_of_dms : dms -> float
(** [deg_of_dms dms] 
    converts degrees, minutes and seconds to decimal degrees. *)

val dms_of_deg : float -> dms
(** [dms_of_deg degrees] 
    converts decimal degrees to degrees, minutes and seconds. *)

val ha_of_ra : 
    Timescale.time -> Timescale.datetime_tz -> float -> Timescale.time
(** [ha_of_ra right_ascension lct geog_long] 
    converts Right Ascension to Hour angle. *)