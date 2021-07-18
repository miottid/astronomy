open Astronomy

module Coordinate_system_test = struct
  let dms_of_degrees () =
    Coordinate_system.dms_of_degrees 182.5241667
    = { degrees = 182.; minutes = 31.; seconds = 27. }

  let degrees_of_dms () =
    Util.approx_equal
      (Coordinate_system.degrees_of_dms
         { degrees = 182.; minutes = 31.; seconds = 27. })
      182.5241667
end

let dms_of_degrees () =
  Alcotest.(check bool)
    "dms_of_degrees" true
    (Coordinate_system_test.dms_of_degrees ())

let degrees_of_dms () =
  Alcotest.(check bool)
    "degrees_of_dms" true
    (Coordinate_system_test.degrees_of_dms ())

let test_set =
  [
    ("dms_of_degrees", `Quick, dms_of_degrees);
    ("degrees_of_dms", `Quick, degrees_of_dms);
  ]
