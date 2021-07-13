open Lib

let () =
  let julian_date = Time.julian_of_greenwich 19.75 6 2009 in
  let day, month, year = Time.greenwich_of_julian julian_date in
  Printf.printf "%f -> %f/%d/%d\n" julian_date day month year;

  let hours, minutes, seconds = Time.hms_of_decimal 18.52416667 in
  Printf.printf "%d:%d:%d\n" hours minutes seconds;

  let decimal = Time.decimal_of_hms 4. 40. 5.23 in
  Printf.printf "Decimal: %f\n" decimal
