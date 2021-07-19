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
end

let dms_of_deg () =
  Alcotest.(check bool) "dms_of_deg" true (Coordinate_system_test.dms_of_deg ())

let deg_of_dms () =
  Alcotest.(check bool) "deg_of_dms" true (Coordinate_system_test.deg_of_dms ())

let ha_of_ra () =
  Alcotest.(check bool) "ha_of_ra" true (Coordinate_system_test.ha_of_ra ())

let test_set =
  [
    ("dms_of_deg", `Quick, dms_of_deg);
    ("deg_of_dms", `Quick, deg_of_dms);
    ("ha_of_ra", `Quick, ha_of_ra);
  ]
