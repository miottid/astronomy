open Lib

let () =
  let julian_date = Time.greenwich_to_julian_date 19.75 6 2009 in
  let day, month, year = Time.julian_to_greenwich_date julian_date in
  Printf.printf "%f -> %f/%d/%d\n" julian_date day month year;

  let hours, minutes, seconds = Time.decimal_to_hms 18.52416667 in
  Printf.printf "%d:%d:%d\n" hours minutes seconds
