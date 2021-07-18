open Astronomy

let approx_equal () =
  Alcotest.(check bool)
    "approx_equal" true
    (Util.approx_equal Float.pi 3.141593)

let roundn () =
  Alcotest.(check @@ float @@ epsilon_float)
    "roundn" 3.14 (Util.roundn Float.pi 2)

let test_set =
  [ ("approx_equal", `Quick, approx_equal); ("roundn", `Quick, roundn) ]
