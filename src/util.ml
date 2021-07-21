let epsilon = 1e-6

let approx_equal a b = Float.abs (a -. b) < epsilon

let roundn f n = Float.round (f *. (10. ** float_of_int n)) /. 100.

let degrees_of_radians radians = radians *. 180. /. Float.pi

let radians_of_degrees degrees = degrees *. Float.pi /. 180.

let deg_of_ha ha = ha *. 15.

let ha_of_deg deg = deg /. 15.
