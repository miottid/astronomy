open Astronomy

module Coordinate_system_test = struct
  let dms_of_degrees () =
    Coordinate_system.dms_of_degrees 182.5241667
    = { degrees = 182.; minutes = 31.; seconds = 27. }
end

let dms_of_degrees () =
  Alcotest.(check bool)
    "dms_of_degrees" true
    (Coordinate_system_test.dms_of_degrees ())

let test_set = [ ("dms_of_degrees", `Quick, dms_of_degrees) ]
