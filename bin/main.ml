open Lib

let () =
  let julian_date = Time.greenwich_to_julian_date 19.75 6 2009 in
  Printf.printf "Julian date: %f\n" julian_date;

  let day, month, year = Time.julian_to_greenwich_date julian_date in
  Printf.printf "%f/%d/%d\n" day month year
