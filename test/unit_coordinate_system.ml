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
        geog_lat = 52.;
      }
    = {
        azimuth = { degrees = 283.; minutes = 16.; seconds = 15.7 };
        altitude = { degrees = 19.; minutes = 20.; seconds = 3.64 };
        geog_lat = 52.;
      }

  let equatorial_of_horizon () =
    Coordinate_system.equatorial_of_horizon
      {
        azimuth = { degrees = 283.; minutes = 16.; seconds = 15.7 };
        altitude = { degrees = 19.; minutes = 20.; seconds = 3.64 };
        geog_lat = 52.;
      }
    = {
        hours_angle = { hours = 5.; minutes = 51.; seconds = 44. };
        declination = { degrees = 23.; minutes = 13.; seconds = 10. };
        geog_lat = 52.;
      }

  let equatorial_of_ecliptic () =
    Coordinate_system.equatorial_of_ecliptic
      { degrees = 139.; minutes = 41.; seconds = 10. }
      { degrees = 4.; minutes = 52.; seconds = 31. }
      { day = 6.; month = 7; year = 2009 }
    = ( { hours = 9.; minutes = 34.; seconds = 53.32 },
        { degrees = 19.; minutes = 32.; seconds = 6.01 } )
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

let mean_obliquity_of_ecliptic () =
  Alcotest.(check @@ float @@ 1e-8)
    "mean_obliquity_of_ecliptic" 23.43805531
    (Coordinate_system.mean_obliquity_of_ecliptic
       { day = 6.; month = 7; year = 2009 })

let equatorial_of_ecliptic () =
  Alcotest.(check bool)
    "equatorial_of_ecliptic" true
    (Coordinate_system_test.equatorial_of_ecliptic ())

let test_set =
  [
    ("dms_of_deg", `Quick, dms_of_deg);
    ("deg_of_dms", `Quick, deg_of_dms);
    ("ha_of_ra", `Quick, ha_of_ra);
    ("ra_of_ha", `Quick, ra_of_ha);
    ("horizon_of_equatorial", `Quick, horizon_of_equatorial);
    ("equatorial_of_horizon", `Quick, equatorial_of_horizon);
    ("mean_obliquity_of_ecliptic", `Quick, mean_obliquity_of_ecliptic);
    ("equatorial_of_ecliptic", `Quick, equatorial_of_ecliptic);
  ]
