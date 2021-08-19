type dms = { degrees : float; minutes : float; seconds : float }

type horizon_coord = { azimuth : dms; altitude : dms }

type ha_coord = { hours_angle : Timescale.hms; declination : dms }

type deg_coord = { longitude : dms; latitude : dms }

type nutation = { longitude : float; obliquity : float }

type rise_set_status = { time : Timescale.hms; azimuth : float }

type rise_set = { rise : rise_set_status; set : rise_set_status }

let pp_dms dms =
  Printf.sprintf "%.8fº %.8fm %.8fs" dms.degrees dms.minutes dms.seconds

let deg_of_dms dms =
  let a = Float.abs dms.seconds /. 60. in
  let b = (Float.abs dms.minutes +. a) /. 60. in
  let c = Float.abs dms.degrees +. b in
  if dms.seconds < 0. || dms.minutes < 0. || dms.seconds < 0. then ~-.c else c

let dms_of_deg deg =
  let udec = Float.abs deg in
  let total_seconds = udec *. 3600. in
  let seconds = Float.round (mod_float total_seconds 60. *. 100.) /. 100. in
  let corrected_seconds = if seconds = 60. then 0. else seconds
  and corrected_remainder =
    if seconds = 60. then total_seconds +. 60. else total_seconds
  in
  let minutes = truncate (corrected_remainder /. 60.) mod 60
  and unsigned_degrees = truncate (corrected_remainder /. 3600.) in
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
  let lst = Timescale.hours_of_hms (Timescale.lst_of_gst (gst, geog_long))
  and ra = Timescale.hours_of_hms ra in
  let h1 = lst -. ra in
  let h = if h1 < 0. then 24. +. h1 else h1 in
  Timescale.hms_of_hours h

let ra_of_ha = ha_of_ra

let horizon_of_equatorial equatorial geog_lat =
  let h = Timescale.hours_of_hms equatorial.hours_angle in
  let h_degs = h *. 15. in
  let h_rads = Util.radians_of_degrees h_degs in
  let dec_degs = deg_of_dms equatorial.declination in
  let dec_rads = Util.radians_of_degrees dec_degs
  and lat_rads = Util.radians_of_degrees geog_lat in
  let sin_a =
    (Float.sin dec_rads *. Float.sin lat_rads)
    +. (Float.cos dec_rads *. Float.cos lat_rads *. Float.cos h_rads)
  in
  let a_rads = Float.asin sin_a in
  let a_degs = Util.degrees_of_radians a_rads
  and y = ~-.(Float.cos dec_rads) *. Float.cos lat_rads *. Float.sin h_rads
  and x = Float.sin dec_rads -. (Float.sin lat_rads *. sin_a) in
  let a = Float.atan2 y x in
  let b = Util.degrees_of_radians a in
  let az_degs = b -. (360. *. Float.floor (b /. 360.)) in
  let az = dms_of_deg az_degs and alt = dms_of_deg a_degs in
  { azimuth = az; altitude = alt }

let equatorial_of_horizon (horizon : horizon_coord) geog_lat =
  let az_degs = deg_of_dms horizon.azimuth
  and alt_degs = deg_of_dms horizon.altitude in
  let alt_rads = Util.radians_of_degrees alt_degs
  and lat_rads = Util.radians_of_degrees geog_lat in
  let az_rads = Util.radians_of_degrees az_degs in
  let sin_dec =
    (Float.sin alt_rads *. Float.sin lat_rads)
    +. (Float.cos alt_rads *. Float.cos lat_rads *. Float.cos az_rads)
  in
  let dec_rads = Float.asin sin_dec in
  let dec_degs = Util.degrees_of_radians dec_rads
  and y = ~-.(Float.cos alt_rads) *. Float.cos lat_rads *. Float.sin az_rads
  and x = Float.sin alt_rads -. (Float.sin lat_rads *. sin_dec) in
  let a = Float.atan2 y x in
  let b = Util.degrees_of_radians a in
  let ha_degs = b -. (360. *. Float.floor (b /. 360.)) in
  let ha_hours = Util.hours_of_degrees ha_degs in
  {
    hours_angle = Timescale.hms_of_hours ha_hours;
    declination = dms_of_deg dec_degs;
  }

let nutation_of_date date =
  let jd = Timescale.julian_of_date date in
  let t = (jd -. 2415020.) /. 36525. in
  let a_deg = 100.0021358 *. t in
  let l1_deg = 279.6967 +. (0.000303 *. t *. t) in
  let l_deg = l1_deg +. (360. *. (a_deg -. Float.floor a_deg)) in
  let l_deg = l_deg -. (360. *. Float.floor (l_deg /. 360.)) in
  let l_rad = Util.radians_of_degrees l_deg and b_deg = 5.372617 *. t in
  let n_deg = 259.1833 -. (360. *. (b_deg -. Float.floor b_deg)) in
  let n_deg = n_deg -. (360. *. Float.floor (n_deg /. 360.)) in
  let n_rad = Util.radians_of_degrees n_deg in
  let nut_long_arcsec =
    (~-.17.2 *. Float.sin n_rad) -. (1.3 *. Float.sin (2. *. l_rad))
  and nut_obliq_arcsec =
    (9.2 *. Float.cos n_rad) +. (0.5 *. Float.cos (2. *. l_rad))
  in
  {
    longitude = nut_long_arcsec /. 3600.;
    obliquity = nut_obliq_arcsec /. 3600.;
  }

let mean_obliquity_of_ecliptic date =
  let jd = Timescale.julian_of_date date in
  let mjd = jd -. 2451545. in
  let t = mjd /. 36525. in
  let de = t *. (46.815 +. (t *. (0.0006 -. (t *. 0.00181)))) in
  let de = de /. 3600. in
  23.439292 -. de

let equatorial_of_ecliptic (ecliptic : deg_coord) date =
  let eclon_deg = deg_of_dms ecliptic.longitude
  and eclat_deg = deg_of_dms ecliptic.latitude in
  let eclon_rad = Util.radians_of_degrees eclon_deg
  and eclat_rad = Util.radians_of_degrees eclat_deg in
  let nutation = nutation_of_date date in
  let obliq_deg = mean_obliquity_of_ecliptic date +. nutation.obliquity in
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
  and x = Float.cos eclon_rad in
  let ra_rad = Float.atan2 y x in
  let ra_deg = Util.degrees_of_radians ra_rad in
  let ra_deg = ra_deg -. (360. *. Float.floor (ra_deg /. 360.)) in
  let ra_hours = Util.hours_of_degrees ra_deg in
  let ra = Timescale.hms_of_hours ra_hours and dec = dms_of_deg dec_deg in
  { hours_angle = ra; declination = dec }

let ecliptic_of_equatorial (equatorial : ha_coord) date =
  let ra_deg =
    Util.degrees_of_hours (Timescale.hours_of_hms equatorial.hours_angle)
  and dec_deg = deg_of_dms equatorial.declination in
  let ra_rad = Util.radians_of_degrees ra_deg
  and dec_rad = Util.radians_of_degrees dec_deg in
  let nutation = nutation_of_date date in
  let obliq_deg = mean_obliquity_of_ecliptic date +. nutation.obliquity in
  let obliq_rad = Util.radians_of_degrees obliq_deg in
  let sin_ecl_lat =
    (Float.sin dec_rad *. Float.cos obliq_rad)
    -. (Float.cos dec_rad *. Float.sin obliq_rad *. Float.sin ra_rad)
  in
  let ecl_lat_rad = Float.asin sin_ecl_lat in
  let ecl_lat_deg = Util.degrees_of_radians ecl_lat_rad in
  let y =
    (Float.sin ra_rad *. Float.cos obliq_rad)
    +. (Float.tan dec_rad *. Float.sin obliq_rad)
  and x = Float.cos ra_rad in
  let ecl_long_rad = Float.atan2 y x in
  let ecl_long_deg = Util.degrees_of_radians ecl_long_rad in
  let ecl_long_deg =
    ecl_long_deg -. (360. *. Float.floor (ecl_long_deg /. 360.))
  in
  { longitude = dms_of_deg ecl_long_deg; latitude = dms_of_deg ecl_lat_deg }

let galactic_of_equatorial (equatorial : ha_coord) =
  let ra_deg =
    Util.degrees_of_hours (Timescale.hours_of_hms equatorial.hours_angle)
  and dec_deg = deg_of_dms equatorial.declination in
  let ra_rad = Util.radians_of_degrees ra_deg
  and dec_rad = Util.radians_of_degrees dec_deg in
  let sinb =
    Float.cos dec_rad
    *. Float.cos (Util.radians_of_degrees 27.4)
    *. Float.cos (ra_rad -. Util.radians_of_degrees 192.25)
    +. (Float.sin dec_rad *. Float.sin (Util.radians_of_degrees 27.4))
  in
  let b_rad = Float.asin sinb in
  let b_deg = Util.degrees_of_radians b_rad in
  let y = Float.sin dec_rad -. (sinb *. Float.sin (Util.radians_of_degrees 27.4))
  and x =
    Float.cos dec_rad
    *. Float.sin (ra_rad -. Util.radians_of_degrees 192.25)
    *. Float.cos (Util.radians_of_degrees 27.4)
  in
  let long_deg = Util.degrees_of_radians (Float.atan2 y x) +. 33. in
  let long_deg = long_deg -. (360. *. Float.floor (long_deg /. 360.)) in
  { latitude = dms_of_deg b_deg; longitude = dms_of_deg long_deg }

let equatorial_of_galactic (galactic : deg_coord) =
  let g_long = deg_of_dms galactic.longitude
  and g_lat = deg_of_dms galactic.latitude in
  let g_long_rad = Util.radians_of_degrees g_long
  and g_lat_rad = Util.radians_of_degrees g_lat in
  let sin_dec =
    Float.cos g_lat_rad
    *. Float.cos (Util.radians_of_degrees 27.4)
    *. Float.sin (g_long_rad -. Util.radians_of_degrees 33.)
    +. (Float.sin g_lat_rad *. Float.sin (Util.radians_of_degrees 27.4))
  in
  let dec_rad = Float.asin sin_dec in
  let dec_deg = Util.degrees_of_radians dec_rad
  and y =
    Float.cos g_lat_rad *. Float.cos (g_long_rad -. Util.radians_of_degrees 33.)
  and x =
    (Float.sin g_lat_rad *. Float.cos (Util.radians_of_degrees 27.4))
    -. Float.cos g_lat_rad
       *. Float.sin (Util.radians_of_degrees 27.4)
       *. Float.sin (g_long_rad -. Util.radians_of_degrees 33.)
  in
  let ra_deg = Util.degrees_of_radians (Float.atan2 y x) +. 192.25 in
  let ra_deg = ra_deg -. (360. *. Float.floor (ra_deg /. 360.)) in
  let ra_hours = Util.hours_of_degrees ra_deg in
  {
    hours_angle = Timescale.hms_of_hours ra_hours;
    declination = dms_of_deg dec_deg;
  }

let angle_between_objects object1 object2 =
  let ra_long_1_dec = Timescale.hours_of_hms object1.hours_angle in
  let ra_long_1_deg = Util.degrees_of_hours ra_long_1_dec in
  let ra_long_1_rad = Util.radians_of_degrees ra_long_1_deg in
  let dec_lat_1_deg = deg_of_dms object1.declination in
  let dec_lat_1_rad = Util.radians_of_degrees dec_lat_1_deg in
  let ra_long_2_dec = Timescale.hours_of_hms object2.hours_angle in
  let ra_long_2_deg = Util.degrees_of_hours ra_long_2_dec in
  let ra_long_2_rad = Util.radians_of_degrees ra_long_2_deg in
  let dec_lat_2_deg = deg_of_dms object2.declination in
  let dec_lat_2_rad = Util.radians_of_degrees dec_lat_2_deg in
  let cos_d =
    (Float.sin dec_lat_1_rad *. Float.sin dec_lat_2_rad)
    +. Float.cos dec_lat_1_rad *. Float.cos dec_lat_2_rad
       *. Float.cos (ra_long_1_rad -. ra_long_2_rad)
  in
  let d_rad = Float.acos cos_d in
  let d_deg = Util.degrees_of_radians d_rad in
  dms_of_deg d_deg

let rising_setting ra date geog_long geog_lat vertical_shift =
  let ra_hours = Timescale.hours_of_hms ra.hours_angle
  and dec_rad = Util.radians_of_degrees (deg_of_dms ra.declination)
  and vertical_displ_rad = Util.radians_of_degrees vertical_shift
  and geo_lat_rad = Util.radians_of_degrees geog_lat in
  let cos_h =
    ~-.((Float.sin vertical_displ_rad
        +. (Float.sin geo_lat_rad *. Float.sin dec_rad))
       /. (Float.cos geo_lat_rad *. Float.cos dec_rad))
  in
  let h_hours =
    Util.hours_of_degrees (Util.degrees_of_radians (Float.acos cos_h))
  in
  let lst_rise_hours =
    ra_hours -. h_hours -. (24. *. Float.floor ((ra_hours -. h_hours) /. 24.))
  and lst_set_hours =
    ra_hours +. h_hours -. (24. *. Float.floor ((ra_hours +. h_hours) /. 24.))
  and a_rad =
    Float.acos
      ((Float.sin dec_rad
       +. (Float.sin vertical_displ_rad *. Float.sin geo_lat_rad))
      /. (Float.cos vertical_displ_rad *. Float.cos geo_lat_rad))
  in
  let a_deg = Util.degrees_of_radians a_rad in
  let az_rise = a_deg -. (360. *. Float.floor (a_deg /. 360.))
  and az_set = 360. -. a_deg -. (360. *. Float.floor ((360. -. a_deg) /. 360.))
  and ut_rise =
    Timescale.gst_of_lst
      ({ hours = lst_rise_hours; minutes = 0.; seconds = 0. }, geog_long)
  in
  let ut_rise =
    Timescale.hours_of_hms (Timescale.ut_of_gst { date; time = ut_rise })
  and ut_set =
    Timescale.gst_of_lst
      ({ hours = lst_set_hours; minutes = 0.; seconds = 0. }, geog_long)
  in
  let ut_set =
    Timescale.hours_of_hms (Timescale.ut_of_gst { date; time = ut_set })
  in
  let ut_rise_adjusted_hours = ut_rise +. 0.008333
  and ut_set_adjusted_hours = ut_set +. 0.008333 in
  let never_rise = cos_h > 1. and circumpolar = cos_h < 1. in
  if not (never_rise && circumpolar) then
    let ut_rise = Timescale.hms_of_hours ut_rise_adjusted_hours
    and ut_set = Timescale.hms_of_hours ut_set_adjusted_hours in
    Some
      {
        rise = { time = ut_rise; azimuth = Util.roundn az_rise 2 };
        set = { time = ut_set; azimuth = Util.roundn az_set 2 };
      }
  else None

let low_precision_precession coordinate epoch1 epoch2 =
  let ra_1_rad =
    Util.radians_of_degrees
      (Util.degrees_of_hours (Timescale.hours_of_hms coordinate.hours_angle))
  and dec_1_rad = Util.radians_of_degrees (deg_of_dms coordinate.declination)
  and t_centuries = (Timescale.julian_of_date epoch1 -. 2415020.) /. 36525. in
  let m_sec = 3.07234 +. (0.00186 *. t_centuries)
  and n_arcsec = 20.0468 -. (0.0085 *. t_centuries)
  and n_years =
    (Timescale.julian_of_date epoch2 -. Timescale.julian_of_date epoch1)
    /. 365.25
  in
  let s_1_hours =
    (m_sec +. (n_arcsec *. Float.sin ra_1_rad *. Float.tan dec_1_rad /. 15.))
    *. n_years /. 3600.
  in
  let ra_2_hours = Timescale.hours_of_hms coordinate.hours_angle +. s_1_hours
  and s_2_deg = n_arcsec *. Float.cos ra_1_rad *. n_years /. 3600. in
  let dec_2_deg = deg_of_dms coordinate.declination +. s_2_deg in
  {
    hours_angle = Timescale.hms_of_hours ra_2_hours;
    declination = dms_of_deg dec_2_deg;
  }
