type dms = { degrees: float; minutes: float; seconds: float }
(** [dms] represent a coordinate in degrees, minutes and seconds. *)

type horizon_coordinate = { azimuth: dms; altitude: dms }
(** [horizon_coordinate] represent the Horizon coordinate. *)

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
(** [ha_of_ra ra lct geog_long] converts Right Ascension to Hour angle. *)

val ra_of_ha :
    Timescale.time -> Timescale.datetime_tz -> float -> Timescale.time
(** [ra_of_ha ha lct geog_long] converts Hour angle to Right Ascension *)

val horizon_of_equatorial :
    Timescale.time -> dms -> float -> horizon_coordinate
(** [horizon_of_equatorial ha declination geog_lat]
    converts Equatorial to Horizon coordinate. *)