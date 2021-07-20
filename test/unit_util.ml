open Astronomy

let approx_equal () =
  Alcotest.(check bool)
    "approx_equal" true
    (Util.approx_equal Float.pi 3.141593)

let roundn () =
  Alcotest.(check @@ float @@ epsilon_float)
    "roundn" 3.14 (Util.roundn Float.pi 2)

let radians_of_degrees () =
  Alcotest.(check @@ float @@ epsilon_float)
    "radians_of_degrees" 6.283185307179586
    (Util.radians_of_degrees 360.)

let degrees_of_radians () =
  Alcotest.(check @@ float @@ epsilon_float)
    "degrees_of_radians" 572.9577951308232
    (Util.degrees_of_radians 10.)

let test_set =
  [
    ("approx_equal", `Quick, approx_equal);
    ("roundn", `Quick, roundn);
    ("radians_of_degrees", `Quick, radians_of_degrees);
    ("degrees_of_radians", `Quick, degrees_of_radians);
  ]
