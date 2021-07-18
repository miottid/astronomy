let epsilon = 1e-6

let approx_equal a b = Float.abs (a -. b) < epsilon

let roundn f n = Float.round (f *. (10. ** float_of_int n)) /. 100.
