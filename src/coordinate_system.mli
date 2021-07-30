type dms = { degrees: float; minutes: float; seconds: float }
(** [dms] represent a coordinate in degrees, minutes and seconds. *)

type horizon_coord = { azimuth: dms; altitude: dms }
(** [horizon_coord] represent the Horizon coordinate. *)

type ha_coord = { hours_angle: Timescale.time; declination: dms }
(** [ha_coord] represent hours angle coordinate with declination. *)

type deg_coord = { longitude: dms; latitude: dms }
(** [degrees_coord] represent the [latitude] and [longitude]
    expressed in degrees, minutes and seconds. *)

type nutation = { longitude: float; obliquity: float }
(** [nutation] represent the calculated nutation at a specific date. *)

val pp_dms : dms -> string
(** [pp_dms dms] converts degrees, minutes and seconds to
    an human readable string. *)

val deg_of_dms : dms -> float
(** [deg_of_dms dms]
    converts degrees, minutes and seconds to decimal degrees. *)

val dms_of_deg : float -> dms
(** [dms_of_deg degrees] converts decimal degrees to
    degrees, minutes and seconds. *)

val ha_of_ra :
    Timescale.time -> Timescale.datetime_tz -> float -> Timescale.time
(** [ha_of_ra ra lct geog_long] converts Right Ascension to Hour angle. *)

val ra_of_ha :
    Timescale.time -> Timescale.datetime_tz -> float -> Timescale.time
(** [ra_of_ha ha lct geog_long] converts Hour angle to Right Ascension *)

val horizon_of_equatorial : ha_coord -> float -> horizon_coord
(** [horizon_of_equatorial equatorial_coord geog_lat]
    converts Equatorial to Horizon coordinate.
    [geog_lat] is the geographical latitude. *)

val equatorial_of_horizon : horizon_coord -> float -> ha_coord
(** [equatorial_of_horizon horizon_coord geog_lat]
    converts Horizon coordinate to Equatorial coordinate.
    [geog_lat] is the geographical latitude. *)

val nutation_of_date : Timescale.date -> nutation
(** [nutation_of_date date] calculate nutation in
    ecliptic longitude and obliquity.
    Returns the pair (nutation in longitude, nutation in obliquity). *)

val mean_obliquity_of_ecliptic : Timescale.date -> float
(** [mean_obliquity_of_ecliptic date] calculate the obliquity (in degrees)
    of the ecliptic. The angle between the planes of
    the equator and the ecliptic. *)

val equatorial_of_ecliptic : deg_coord -> Timescale.date -> ha_coord
(** [equatorial_of_ecliptic ecliptic_coordinate date]
    converts Ecliptic to Equatorial coordinate. *)

val ecliptic_of_equatorial : ha_coord -> Timescale.date -> deg_coord
(** [ecliptic_of_equatorial equatorial date]
    converts Equatorial coordinate to Ecliptic coordinate. *)

val galactic_of_equatorial : ha_coord -> deg_coord
(** [galactic_of_equatorial deg_coord] 
    converts Equatorial coordinate to Galactic coodinate. *)

val equatorial_of_galactic : deg_coord -> ha_coord
(** [equatorial_of_galactic deg_coord]
    converts Galactic coordinate to Equatorial coordinate. *)