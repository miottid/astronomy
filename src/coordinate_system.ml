type dms = { degrees : float; minutes : float; seconds : float }

type horizon_coord = { azimuth : dms; altitude : dms; geog_lat : float }

type equatorial_coord = {
  hours_angle : Timescale.time;
  declination : dms;
  geog_lat : float;
}

type ecliptic_coord = { longitude : dms; latitude : dms; date : Timescale.date }

type nutation = { longitude : float; obliquity : float }

let pp_dms dms =
  Printf.sprintf "%fº %fm %fs" dms.degrees dms.minutes dms.seconds

let deg_of_dms dms =
  let a = Float.abs dms.seconds /. 60. in
  let b = (Float.abs dms.minutes +. a) /. 60. in
  let c = Float.abs dms.degrees +. b in
  if dms.seconds < 0. || dms.minutes < 0. || dms.seconds < 0. then ~-.c else c

let dms_of_deg deg =
  let udec = Float.abs deg in
  let total_seconds = udec *. 3600. in
  let seconds = Float.round (mod_float total_seconds 60. *. 100.) /. 100. in
  let corrected_seconds = if seconds = 60. then 0. else seconds in
  let corrected_remainder =
    if seconds = 60. then total_seconds +. 60. else total_seconds
  in
  let minutes = truncate (corrected_remainder /. 60.) mod 60 in
  let unsigned_degrees = truncate (corrected_remainder /. 3600.) in
  let signed_degrees =
    if seconds < 0. then -1 * unsigned_degrees else unsigned_degrees
  in
  {
    degrees = float signed_degrees;
    minutes = float minutes;
    seconds = corrected_seconds;
  }

let ha_of_ra ra lct geog_long =
  let ut = Timescale.ut_of_lct lct in
  let gst = Timescale.gst_of_ut ut in
  let lst = Timescale.hours_of_time (Timescale.lst_of_gst (gst, geog_long)) in
  let ra = Timescale.hours_of_time ra in
  let h1 = lst -. ra in
  let h = if h1 < 0. then 24. +. h1 else h1 in
  Timescale.time_of_hours h

let ra_of_ha = ha_of_ra

let horizon_of_equatorial equatorial =
  let h = Timescale.hours_of_time equatorial.hours_angle in
  let h_degs = h *. 15. in
  let h_rads = Util.radians_of_degrees h_degs in
  let dec_degs = deg_of_dms equatorial.declination in
  let dec_rads = Util.radians_of_degrees dec_degs in
  let lat_rads = Util.radians_of_degrees equatorial.geog_lat in
  let sin_a =
    (Float.sin dec_rads *. Float.sin lat_rads)
    +. (Float.cos dec_rads *. Float.cos lat_rads *. Float.cos h_rads)
  in
  let a_rads = Float.asin sin_a in
  let a_degs = Util.degrees_of_radians a_rads in
  let y = ~-.(Float.cos dec_rads) *. Float.cos lat_rads *. Float.sin h_rads in
  let x = Float.sin dec_rads -. (Float.sin lat_rads *. sin_a) in
  let a = Float.atan2 y x in
  let b = Util.degrees_of_radians a in
  let az_degs = b -. (360. *. Float.floor (b /. 360.)) in
  let az = dms_of_deg az_degs in
  let alt = dms_of_deg a_degs in
  { azimuth = az; altitude = alt; geog_lat = equatorial.geog_lat }

let equatorial_of_horizon horizon =
  let az_degs = deg_of_dms horizon.azimuth in
  let alt_degs = deg_of_dms horizon.altitude in
  let alt_rads = Util.radians_of_degrees alt_degs in
  let lat_rads = Util.radians_of_degrees horizon.geog_lat in
  let az_rads = Util.radians_of_degrees az_degs in
  let sin_dec =
    (Float.sin alt_rads *. Float.sin lat_rads)
    +. (Float.cos alt_rads *. Float.cos lat_rads *. Float.cos az_rads)
  in
  let dec_rads = Float.asin sin_dec in
  let dec_degs = Util.degrees_of_radians dec_rads in
  let y = ~-.(Float.cos alt_rads) *. Float.cos lat_rads *. Float.sin az_rads in
  let x = Float.sin alt_rads -. (Float.sin lat_rads *. sin_dec) in
  let a = Float.atan2 y x in
  let b = Util.degrees_of_radians a in
  let ha_degs = b -. (360. *. Float.floor (b /. 360.)) in
  let ha_hours = Util.ha_of_deg ha_degs in
  {
    hours_angle = Timescale.time_of_hours ha_hours;
    declination = dms_of_deg dec_degs;
    geog_lat = horizon.geog_lat;
  }

let nutation_of_date date =
  let jd = Timescale.julian_of_greenwich date in
  let t = (jd -. 2415020.) /. 36525. in
  let a_deg = 100.0021358 *. t in
  let l1_deg = 279.6967 +. (0.000303 *. t *. t) in
  let l_deg = l1_deg +. (360. *. (a_deg -. Float.floor a_deg)) in
  let l_deg = l_deg -. (360. *. Float.floor (l_deg /. 360.)) in
  let l_rad = Util.radians_of_degrees l_deg in
  let b_deg = 5.372617 *. t in
  let n_deg = 259.1833 -. (360. *. (b_deg -. Float.floor b_deg)) in
  let n_deg = n_deg -. (360. *. Float.floor (n_deg /. 360.)) in
  let n_rad = Util.radians_of_degrees n_deg in
  let nut_long_arcsec =
    (~-.17.2 *. Float.sin n_rad) -. (1.3 *. Float.sin (2. *. l_rad))
  in
  let nut_obliq_arcsec =
    (9.2 *. Float.cos n_rad) +. (0.5 *. Float.cos (2. *. l_rad))
  in
  {
    longitude = nut_long_arcsec /. 3600.;
    obliquity = nut_obliq_arcsec /. 3600.;
  }

let mean_obliquity_of_ecliptic date =
  let jd = Timescale.julian_of_greenwich date in
  let mjd = jd -. 2451545. in
  let t = mjd /. 36525. in
  let de = t *. (46.815 +. (t *. (0.0006 -. (t *. 0.00181)))) in
  let de = de /. 3600. in
  23.439292 -. de

let equatorial_of_ecliptic (ecliptic : ecliptic_coord) =
  let eclon_deg = deg_of_dms ecliptic.longitude in
  let eclat_deg = deg_of_dms ecliptic.latitude in
  let eclon_rad = Util.radians_of_degrees eclon_deg in
  let eclat_rad = Util.radians_of_degrees eclat_deg in
  let nutation = nutation_of_date ecliptic.date in
  let obliq_deg =
    mean_obliquity_of_ecliptic ecliptic.date +. nutation.obliquity
  in
  let obliq_rad = Util.radians_of_degrees obliq_deg in
  let sin_dec =
    (Float.sin eclat_rad *. Float.cos obliq_rad)
    +. (Float.cos eclat_rad *. Float.sin obliq_rad *. Float.sin eclon_rad)
  in
  let dec_rad = Float.asin sin_dec in
  let dec_deg = Util.degrees_of_radians dec_rad in
  let y =
    (Float.sin eclon_rad *. Float.cos obliq_rad)
    -. (Float.tan eclat_rad *. Float.sin obliq_rad)
  in
  let x = Float.cos eclon_rad in
  let ra_rad = Float.atan2 y x in
  let ra_deg = Util.degrees_of_radians ra_rad in
  let ra_deg = ra_deg -. (360. *. Float.floor (ra_deg /. 360.)) in
  let ra_hours = Util.ha_of_deg ra_deg in
  let ra = Timescale.time_of_hours ra_hours in
  let dec = dms_of_deg dec_deg in
  {
    hours_angle = ra;
    declination = dec;
    geog_lat = deg_of_dms ecliptic.latitude;
  }
