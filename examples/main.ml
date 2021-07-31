open Astronomy

let () =
  let julian =
    Timescale.julian_of_greenwich { day = 19.75; month = 6; year = 2009 }
  in
  let greenwich = Timescale.greenwich_of_julian julian in
  Printf.printf "%f -> %s\n" julian (Timescale.pp_date greenwich);

  let t = Timescale.hms_of_hours 18.52416667 in
  Printf.printf "%s\n" (Timescale.pp_hms t);

  let decimal =
    Timescale.hours_of_hms { hours = 4.; minutes = 40.; seconds = 5.23 }
  in
  Printf.printf "Decimal: %.8f\n" decimal;

  let t =
    Timescale.ut_of_gst
      {
        date = { day = 22.; month = 4; year = 1980 };
        time = { hours = 4.; minutes = 40.; seconds = 5.23 };
      }
  in
  Printf.printf "t: %s\n" (Timescale.pp_hms t)
