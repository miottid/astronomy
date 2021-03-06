open Astronomy

module Coordinate_system_test = struct
  let dms_of_deg () =
    Coordinate_system.dms_of_deg 182.5241667
    = { degrees = 182.; minutes = 31.; seconds = 27. }

  let deg_of_dms () =
    Util.approx_equal
      (Coordinate_system.deg_of_dms
         { degrees = 182.; minutes = 31.; seconds = 27. })
      182.5241667

  let ha_of_ra () =
    let r =
      Coordinate_system.ha_of_ra
        { hours = 18.; minutes = 32.; seconds = 21. }
        {
          datetime =
            {
              date = { day = 22.; month = 4; year = 1980 };
              time = { hours = 14.; minutes = 36.; seconds = 51.67 };
            };
          tzoffset = -4.;
          daylight = 0.;
        }
        ~-.64.
    in
    r = { hours = 9.; minutes = 52.; seconds = 23.66 }

  let ra_of_ha () =
    Coordinate_system.ra_of_ha
      { hours = 9.; minutes = 52.; seconds = 23.66 }
      {
        datetime =
          {
            date = { day = 22.; month = 4; year = 1980 };
            time = { hours = 14.; minutes = 36.; seconds = 51.67 };
          };
        tzoffset = -4.;
        daylight = 0.;
      }
      ~-.64.
    = { hours = 18.; minutes = 32.; seconds = 21. }

  let horizon_of_equatorial () =
    Coordinate_system.horizon_of_equatorial
      {
        hours_angle = { hours = 5.; minutes = 51.; seconds = 44. };
        declination = { degrees = 23.; minutes = 13.; seconds = 10. };
      }
      52.
    = {
        azimuth = { degrees = 283.; minutes = 16.; seconds = 15.7 };
        altitude = { degrees = 19.; minutes = 20.; seconds = 3.64 };
      }

  let equatorial_of_horizon () =
    Coordinate_system.equatorial_of_horizon
      {
        azimuth = { degrees = 283.; minutes = 16.; seconds = 15.7 };
        altitude = { degrees = 19.; minutes = 20.; seconds = 3.64 };
      }
      52.
    = {
        hours_angle = { hours = 5.; minutes = 51.; seconds = 44. };
        declination = { degrees = 23.; minutes = 13.; seconds = 10. };
      }

  let nutation_of_date () =
    let nutation =
      Coordinate_system.nutation_of_date { day = 1.; month = 9; year = 1988 }
    in
    Util.approx_equal 0.001525808 nutation.longitude
    && Util.approx_equal 0.0025671 nutation.obliquity

  let equatorial_of_ecliptic () =
    Coordinate_system.equatorial_of_ecliptic
      {
        longitude = { degrees = 139.; minutes = 41.; seconds = 10. };
        latitude = { degrees = 4.; minutes = 52.; seconds = 31. };
      }
      { day = 6.; month = 7; year = 2009 }
    = {
        hours_angle = { hours = 9.; minutes = 34.; seconds = 53.4 };
        declination = { degrees = 19.; minutes = 32.; seconds = 8.56 };
      }

  let ecliptic_of_equatorial () =
    Coordinate_system.ecliptic_of_equatorial
      {
        hours_angle = { hours = 9.; minutes = 34.; seconds = 53.4 };
        declination = { degrees = 19.; minutes = 32.; seconds = 8.56 };
      }
      { day = 6.; month = 7; year = 2009 }
    = {
        longitude = { degrees = 139.; minutes = 41.; seconds = 9.95 };
        latitude = { degrees = 4.; minutes = 52.; seconds = 30.98 };
      }

  let galactic_of_equatorial () =
    Coordinate_system.galactic_of_equatorial
      {
        hours_angle = { hours = 10.; minutes = 21.; seconds = 0. };
        declination = { degrees = 10.; minutes = 3.; seconds = 11. };
      }
    = {
        latitude = { degrees = 51.; minutes = 7.; seconds = 20.16 };
        longitude = { degrees = 232.; minutes = 14.; seconds = 52.38 };
      }

  let equatorial_of_galactic () =
    Coordinate_system.equatorial_of_galactic
      {
        latitude = { degrees = 51.; minutes = 7.; seconds = 20.16 };
        longitude = { degrees = 232.; minutes = 14.; seconds = 52.38 };
      }
    = {
        hours_angle = { hours = 10.; minutes = 21.; seconds = 0. };
        declination = { degrees = 10.; minutes = 3.; seconds = 11. };
      }

  let angle_between_objects () =
    Coordinate_system.angle_between_objects
      {
        hours_angle = { hours = 5.; minutes = 13.; seconds = 31.7 };
        declination = { degrees = -8.; minutes = 13.; seconds = 30. };
      }
      {
        hours_angle = { hours = 6.; minutes = 44.; seconds = 13.4 };
        declination = { degrees = -16.; minutes = 41.; seconds = 11. };
      }
    = { degrees = 23.; minutes = 40.; seconds = 25.86 }

  let rising_setting () =
    Coordinate_system.rising_setting
      {
        hours_angle = { hours = 23.; minutes = 39.; seconds = 20. };
        declination = { degrees = 21.; minutes = 42.; seconds = 0. };
      }
      { day = 24.; month = 8; year = 2010 }
      64. 30. 0.5667
    = Some
        {
          rise =
            {
              time = { hours = 14.; minutes = 16.; seconds = 48.01 };
              azimuth = 64.36;
            };
          set =
            {
              time = { hours = 4.; minutes = 10.; seconds = 31.17 };
              azimuth = 295.64;
            };
        }

  let low_precision_precession () =
    Coordinate_system.low_precision_precession
      {
        hours_angle = { hours = 9.; minutes = 10.; seconds = 43. };
        declination = { degrees = 14.; minutes = 23.; seconds = 25. };
      }
      { day = 0.923; month = 1; year = 1950 }
      { day = 1.; month = 6; year = 1979 }
    = {
        hours_angle = { hours = 9.; minutes = 12.; seconds = 20.18 };
        declination = { degrees = 14.; minutes = 16.; seconds = 9.12 };
      }
end

let dms_of_deg () =
  Alcotest.(check bool) "dms_of_deg" true (Coordinate_system_test.dms_of_deg ())

let deg_of_dms () =
  Alcotest.(check bool) "deg_of_dms" true (Coordinate_system_test.deg_of_dms ())

let ha_of_ra () =
  Alcotest.(check bool) "ha_of_ra" true (Coordinate_system_test.ha_of_ra ())

let ra_of_ha () =
  Alcotest.(check bool) "ra_of_ha" true (Coordinate_system_test.ra_of_ha ())

let horizon_of_equatorial () =
  Alcotest.(check bool)
    "horizon_of_equatorial" true
    (Coordinate_system_test.horizon_of_equatorial ())

let equatorial_of_horizon () =
  Alcotest.(check bool)
    "equatorial_of_horizon" true
    (Coordinate_system_test.equatorial_of_horizon ())

let nutation_of_date () =
  Alcotest.(check bool)
    "nutation_of_date" true
    (Coordinate_system_test.nutation_of_date ())

let mean_obliquity_of_ecliptic () =
  Alcotest.(check @@ float @@ 1e-8)
    "mean_obliquity_of_ecliptic" 23.43805531
    (Coordinate_system.mean_obliquity_of_ecliptic
       { day = 6.; month = 7; year = 2009 })

let equatorial_of_ecliptic () =
  Alcotest.(check bool)
    "equatorial_of_ecliptic" true
    (Coordinate_system_test.equatorial_of_ecliptic ())

let ecliptic_of_equatorial () =
  Alcotest.(check bool)
    "ecliptic_of_equatorial" true
    (Coordinate_system_test.ecliptic_of_equatorial ())

let galactic_of_equatorial () =
  Alcotest.(check bool)
    "galactic_of_equatorial" true
    (Coordinate_system_test.galactic_of_equatorial ())

let equatorial_of_galactic () =
  Alcotest.(check bool)
    "equatorial_of_galactic" true
    (Coordinate_system_test.equatorial_of_galactic ())

let angle_between_objects () =
  Alcotest.(check bool)
    "angle_between_objects" true
    (Coordinate_system_test.angle_between_objects ())

let rising_setting () =
  Alcotest.(check bool)
    "rising_setting" true
    (Coordinate_system_test.rising_setting ())

let low_precision_precession () =
  Alcotest.(check bool)
    "low_precision_precession" true
    (Coordinate_system_test.low_precision_precession ())

let test_set =
  [
    ("dms_of_deg", `Quick, dms_of_deg);
    ("deg_of_dms", `Quick, deg_of_dms);
    ("ha_of_ra", `Quick, ha_of_ra);
    ("ra_of_ha", `Quick, ra_of_ha);
    ("horizon_of_equatorial", `Quick, horizon_of_equatorial);
    ("equatorial_of_horizon", `Quick, equatorial_of_horizon);
    ("nutation_of_date", `Quick, nutation_of_date);
    ("mean_obliquity_of_ecliptic", `Quick, mean_obliquity_of_ecliptic);
    ("equatorial_of_ecliptic", `Quick, equatorial_of_ecliptic);
    ("ecliptic_of_equatorial", `Quick, ecliptic_of_equatorial);
    ("galactic_of_equatorial", `Quick, galactic_of_equatorial);
    ("equatorial_of_galactic", `Quick, equatorial_of_galactic);
    ("angle_between_objects", `Quick, angle_between_objects);
    ("rising_setting", `Quick, rising_setting);
    ("low_precision_precession", `Quick, low_precision_precession);
  ]
