type dms = { degrees : float; minutes : float; seconds : float }

let pp_dms dms =
  Printf.sprintf "%fº %fm %fs" dms.degrees dms.minutes dms.seconds

let degrees_of_dms dms =
  let a = Float.abs dms.seconds /. 60. in
  let b = (Float.abs dms.minutes +. a) /. 60. in
  let c = Float.abs dms.degrees +. b in
  if dms.seconds < 0. || dms.minutes < 0. || dms.seconds < 0. then -1. *. c
  else c

let dms_of_degrees degrees =
  let udec = Float.abs degrees in
  let total_seconds = udec *. 3600. in
  let seconds = Float.round (mod_float total_seconds 60. *. 100.) /. 100. in
  let corrected_seconds =
    if Util.approx_equal seconds 60. then 0. else seconds
  in
  let corrected_remainder =
    if Util.approx_equal seconds 60. then total_seconds +. 60.
    else total_seconds
  in
  let minutes = truncate (corrected_remainder /. 60.) mod 60 in
  let unsigned_degrees = truncate (corrected_remainder /. 3600.) in
  let signed_degrees =
    if seconds < 0. then -1 * unsigned_degrees else unsigned_degrees
  in
  {
    degrees = float signed_degrees;
    minutes = float minutes;
    seconds = corrected_seconds;
  }
