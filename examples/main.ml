open Astronomy

let () =
  let julian =
    Timescale.julian_of_greenwich { day = 19.75; month = 6; year = 2009 }
  in
  let greenwich = Timescale.greenwich_of_julian julian in
  Printf.printf "%f -> %f/%d/%d\n" julian greenwich.day greenwich.month
    greenwich.year;

  let t = Timescale.time_of_hours 18.52416667 in
  Printf.printf "%f:%f:%f\n" t.hours t.minutes t.seconds;

  let decimal =
    Timescale.hours_of_time { hours = 4.; minutes = 40.; seconds = 5.23 }
  in
  Printf.printf "Decimal: %f\n" decimal;

  let t =
    Timescale.ut_of_gst
      {
        date = { day = 22.; month = 4; year = 1980 };
        time = { hours = 4.; minutes = 40.; seconds = 5.23 };
      }
  in
  Printf.printf "t: %.9fh %.9fm %.9fs\n" t.hours t.minutes t.seconds
