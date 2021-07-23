type dms = { degrees: float; minutes: float; seconds: float }
(** [dms] represent a coordinate in degrees, minutes and seconds. *)

type horizon_coord = { azimuth: dms; altitude: dms; geog_lat: float }
(** [horizon_coord] represent the Horizon coordinate. *)

type equatorial_coord = {
    hours_angle: Timescale.time;
    declination: dms;
    geog_lat: float;
}
(** [equatorial_coord] represent the Equatorial coordinate.
    [geog_lat] is the latitude expressed in degrees. *)

type ecliptic_coord = {
    longitude: dms;
    latitude: dms;
    date: Timescale.date;
}
(** [ecliptic_coord] represent the Ecliptic coordinate. *)

type nutation = {
    longitude: float;
    obliquity: float;
}
(** [nutation] represent the calculated nutation at a specific date. *)

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

val nutation_of_date : Timescale.date -> nutation
(** [nutation_of_date date] calculate nutation in
    ecliptic longitude and obliquity.
    Returns the pair (nutation in longitude, nutation in obliquity). *)

val mean_obliquity_of_ecliptic : Timescale.date -> float
(** [mean_obliquity_of_ecliptic date] calculate the obliquity (in degrees)
    of the ecliptic. The angle between the planes of
    the equator and the ecliptic. *)

val equatorial_of_ecliptic : ecliptic_coord -> equatorial_coord
(** [equatorial_of_ecliptic ecliptic_coordinate] 
    converts Ecliptic to Equatorial coordinate. *)
