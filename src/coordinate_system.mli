type dms = { degrees: float; minutes: float; seconds: float }
(** [dms] represent a coordinate in degrees, minutes and seconds. *)

type horizon_coord = { azimuth: dms; altitude: dms; geog_lat: float }
(** [horizon_coord] represent the Horizon coordinate. *)

type equatorial_coord = {
    hours_angle: Timescale.time;
    declination: dms;
    geog_lat: float;
}
(** [equatorial_coord] represent the Equatorial coordinate. *)

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

val horizon_of_equatorial : equatorial_coord -> horizon_coord
(** [horizon_of_equatorial equatorial_coord]
    converts Equatorial to Horizon coordinate. *)

val equatorial_of_horizon : horizon_coord -> equatorial_coord
(** [equatorial_of_horizon horizon_coord]
    converts Horizon coordinate to Equatorial coordinate. *)