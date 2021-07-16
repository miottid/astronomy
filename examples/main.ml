open Astronomy

let () =
  let julian =
    Time.julian_of_greenwich { day = 19.75; month = 6; year = 2009 }
  in
  let greenwich = Time.greenwich_of_julian julian in
  Printf.printf "%f -> %f/%d/%d\n" julian greenwich.day greenwich.month
    greenwich.year;

  let time = Time.time_of_hours 18.52416667 in
  Printf.printf "%f:%f:%f\n" time.hours time.minutes time.seconds;

  let decimal =
    Time.hours_of_time { hours = 4.; minutes = 40.; seconds = 5.23 }
  in
  Printf.printf "Decimal: %f\n" decimal
