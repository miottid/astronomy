open Astro

let () =
  let julian_date = Time.julian_date_of_greenwich (19.75, 6, 2009) in
  let day, month, year = Time.greenwich_date_of_julian julian_date in
  Printf.printf "%f -> %f/%d/%d\n" julian_date day month year;

  let hours, minutes, seconds = Time.hms_of_decimal_hours 18.52416667 in
  Printf.printf "%f:%f:%f\n" hours minutes seconds;

  let decimal = Time.decimal_hours_of_hms (4., 40., 5.23) in
  Printf.printf "Decimal: %f\n" decimal
