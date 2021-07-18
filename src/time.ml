type date = { day : float; month : int; year : int }

type time = { hours : float; minutes : float; seconds : float }

type datetime = { date : date; time : time }

type datetime_tz = { tzoffset : float; daylight : float; datetime : datetime }

let truncate_float f = float (truncate f)

let string_of_month = function
  | 1 -> "January"
  | 2 -> "February"
  | 3 -> "March"
  | 4 -> "April"
  | 5 -> "May"
  | 6 -> "June"
  | 7 -> "July"
  | 8 -> "August"
  | 9 -> "September"
  | 10 -> "October"
  | 11 -> "November"
  | 12 -> "December"
  | _ -> assert false

let string_of_weekday = function
  | 0 -> "Sunday"
  | 1 -> "Monday"
  | 2 -> "Tuesday"
  | 3 -> "Wednesday"
  | 4 -> "Thursday"
  | 5 -> "Friday"
  | 6 -> "Saturday"
  | _ -> assert false

let easter_day year =
  let a = year mod 19 and b = year / 100 and c = year mod 100 in
  let d = b / 4
  and e = b mod 4
  and f = (b + 8) / 25
  and i = c / 4
  and k = c mod 4 in
  let g = (b - f + 1) / 3 in
  let h = ((19 * a) + b - d - g + 15) mod 30 in
  let l = (32 + (2 * e) + (2 * i) - h - k) mod 7 in
  let m = (a + (11 * h) + (22 * l)) / 451 in
  let month_number = (h + l - (7 * m) + 114) / 31
  and day_of_month = ((h + l - (7 * m) + 114) mod 31) + 1 in
  { day = float day_of_month; month = month_number; year }

let julian_of_greenwich date =
  let yd = if date.month < 3 then date.year - 1 else date.year
  and md = if date.month < 3 then date.month + 12 else date.month in
  let a = yd / 100 in
  let b =
    if
      yd > 1582
      || (yd = 1582 && date.month > 10)
      || (yd = 1582 && date.month = 10 && date.day > 15.)
    then 2 - a + (a / 4)
    else 0
  in
  let c =
    if yd < 0 then truncate_float ((365.25 *. float yd) -. 0.75)
    else truncate_float (365.25 *. float yd)
  and d = truncate_float (30.6001 *. (float md +. 1.)) in
  float b +. c +. d +. date.day +. 1720994.5

let greenwich_of_julian julian =
  let julian = julian +. 0.5 in
  let i = truncate_float julian in
  let f = julian -. i in
  let b =
    if i > 2299160. then
      let a = truncate_float ((i -. 1867216.25) /. 36524.25) in
      i +. a -. (a /. 4.) +. 1.
    else i
  in
  let c = b +. 1524. in
  let d = truncate_float ((c -. 122.1) /. 365.25) in
  let e = truncate_float (365.25 *. d) in
  let g = truncate_float ((c -. e) /. 30.6001) in
  let day = c -. e +. f -. float (truncate (30.6001 *. g)) in
  let month = if g < 13.5 then g -. 1. else g -. 13. in
  let year = if month > 2.5 then d -. 4716. else d -. 4715. in
  { day; month = truncate month; year = int_of_float year }

let time_of_hours hours =
  let rounded_hours = Float.round (hours *. 10_000_000.) /. 10_000_000. in
  let total_seconds = truncate (Float.abs rounded_hours *. 3600.) in
  let seconds = total_seconds mod 60 in
  let corrected_seconds = if seconds = 60 then 0 else seconds in
  let corrected_remainder =
    if seconds = 60 then total_seconds + 60 else total_seconds
  in
  let minutes = corrected_remainder / 60 mod 60 in
  let unsigned_hours = corrected_remainder / 3600 in
  let signed_hours =
    if hours < 0. then -1 * unsigned_hours else unsigned_hours
  in
  {
    hours = float signed_hours;
    minutes = float minutes;
    seconds = float corrected_seconds;
  }

let hours_of_time time =
  let a = Float.abs time.seconds /. 60. in
  let b = (Float.abs time.minutes +. a) /. 60. in
  let c = Float.abs time.hours +. b in
  if time.hours < 0. || time.minutes < 0. || time.seconds < 0. then -1. *. c
  else c

let weekday_of_julian julian =
  let jd = truncate_float (julian -. 0.5) +. 0.5 in
  truncate (jd +. 1.5) mod 7

let weekday_of_date date = weekday_of_julian (julian_of_greenwich date)

let ut_of_lct datetime_tz =
  let lct = hours_of_time datetime_tz.datetime.time in
  let ut = lct -. datetime_tz.daylight -. datetime_tz.tzoffset in
  let gday = datetime_tz.datetime.date.day +. (ut /. 24.) in
  let gdate =
    {
      day = gday;
      month = datetime_tz.datetime.date.month;
      year = datetime_tz.datetime.date.year;
    }
  in
  let jd = julian_of_greenwich gdate in
  let gdate = greenwich_of_julian jd in
  let ut = 24. *. (gdate.day -. truncate_float gdate.day) in
  let time = time_of_hours ut in
  {
    date =
      { day = truncate_float gdate.day; month = gdate.month; year = gdate.year };
    time;
  }

let lct_of_ut datetime_tz =
  let ut = hours_of_time datetime_tz.datetime.time in
  let zone_time = ut +. datetime_tz.tzoffset in
  let local_time = zone_time +. datetime_tz.daylight in
  let local_jd =
    julian_of_greenwich datetime_tz.datetime.date +. (local_time /. 24.)
  in
  let gdate = greenwich_of_julian local_jd in
  let int_day = truncate_float gdate.day in
  let lct = 24. *. (gdate.day -. int_day) in
  {
    date = { day = int_day; month = gdate.month; year = gdate.year };
    time = time_of_hours lct;
  }

let gst_of_ut datetime =
  let jd = julian_of_greenwich datetime.date in
  let s = jd -. 2451545. in
  let t = s /. 36525. in
  let t0 = 6.697374558 +. (2400.051336 *. t) +. (0.000025862 *. t *. t) in
  let t0 = t0 -. (24. *. truncate_float (t0 /. 24.)) in
  let ut = hours_of_time datetime.time in
  let a = ut *. 1.002737909 in
  let gst = t0 +. a in
  let gst = gst -. (24. *. truncate_float (gst /. 24.)) in
  time_of_hours gst

let ut_of_gst datetime =
  Printf.printf "\n-- ut_of_gst\n";
  let jd = julian_of_greenwich datetime.date in
  Printf.printf "jd: %.9f\n" jd;
  let s = jd -. 2451545. in
  Printf.printf "s: %f.9\n" s;
  let t = s /. 36525. in
  Printf.printf "t: %.9f\n" t;
  let t0 = 6.697374558 +. (2400.051336 *. t) +. (0.000025862 *. t *. t) in
  Printf.printf "t0: %.9f\n" t0;
  let t0 = t0 -. (24. *. truncate_float (t0 /. 24.)) in
  Printf.printf "t0: %.9f\n" t0;
  let gsthrs = hours_of_time datetime.time in
  Printf.printf "gsthrs: %.9f\n" gsthrs;
  let a = gsthrs -. t0 in
  (* Printf.printf "a: %.9f\n" a; *)
  let b = a -. (24. *. truncate_float (a /. 24.)) in
  (* Printf.printf "b: %.9f\n" b; *)
  let ut = b *. 0.9972695663 in
  (* Printf.printf "ut: %.9f\n" ut; *)
  time_of_hours ut

let%test "ut_of_gst" =
  ut_of_gst
    {
      date = { day = 22.; month = 4; year = 1980 };
      time = { hours = 4.; minutes = 30.; seconds = 0. };
    }
  = { hours = 14.; minutes = 36.; seconds = 51. }
