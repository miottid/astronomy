type date = float * int * int

type hms = float * float * float

let truncate_float f = float_of_int (truncate f)

let check_list f lst =
  List.fold_left (fun acc (input, output) -> acc && f input = output) true lst

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

let date_of_easter year =
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
  (day_of_month, month_number)

let%test "date_of_easter" =
  check_list date_of_easter
    [
      (2019, (21, 4));
      (2020, (12, 4));
      (2021, (4, 4));
      (2022, (17, 4));
      (2023, (9, 4));
      (2024, (31, 3));
      (2025, (20, 4));
    ]

let julian_date_of_greenwich (day, month, year) =
  let yd = if month < 3 then year - 1 else year
  and md = if month < 3 then month + 12 else month in
  let a = yd / 100 in
  let b =
    if
      yd > 1582
      || (yd = 1582 && month > 10)
      || (yd = 1582 && month = 10 && day > 15.)
    then 2 - a + (a / 4)
    else 0
  in
  let c =
    if yd < 0 then truncate_float ((365.25 *. float_of_int yd) -. 0.75)
    else truncate_float (365.25 *. float_of_int yd)
  and d = truncate_float (30.6001 *. (float_of_int md +. 1.)) in
  float_of_int b +. c +. d +. day +. 1720994.5

let%test "julian_date_of_greenwich" =
  check_list julian_date_of_greenwich
    [ ((19.75, 6, 2009), 2455002.25); ((12.625, 7, 2021), 2459408.125) ]

let greenwich_date_of_julian julian =
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
  let day = c -. e +. f -. float_of_int (truncate (30.6001 *. g)) in
  let month = if g < 13.5 then g -. 1. else g -. 13. in
  let year = if month > 2.5 then d -. 4716. else d -. 4715. in
  (day, truncate month, truncate year)

let%test "greenwich_date_of_julian" =
  greenwich_date_of_julian 2455002.25 = (19.75, 6, 2009)

let hms_of_decimal_hours hours =
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
  ( float_of_int signed_hours,
    float_of_int minutes,
    float_of_int corrected_seconds )

let%test "julian_date_of_greenwich" =
  check_list hms_of_decimal_hours
    [ (18.52416667, (18., 31., 27.)); (22.6167, (22., 37., 0.)) ]

let decimal_hours_of_hms (hours, minutes, seconds) =
  let a = Float.abs seconds /. 60. in
  let b = (Float.abs minutes +. a) /. 60. in
  let c = Float.abs hours +. b in
  if hours < 0. || minutes < 0. || seconds < 0. then -1. *. c else c

let weekday_of_julian_date julian =
  let jd = truncate_float (julian -. 0.5) +. 0.5 in
  truncate (jd +. 1.5) mod 7

let%test "weekday_of_julian_date" = weekday_of_julian_date 2455001.5 = 5

let weekday_of_date date =
  weekday_of_julian_date (julian_date_of_greenwich date)

let%test "weekday_of_date" = weekday_of_date (19., 6, 2009) = 5

let ut_of_lct (day, month, year) hms daylight tzoffset =
  let lct = decimal_hours_of_hms hms in
  let ut = lct -. daylight -. tzoffset in
  let gday = day +. (ut /. 24.) in
  let jd = julian_date_of_greenwich (gday, month, year) in
  let gday, gm, gy = greenwich_date_of_julian jd in
  let ut = 24. *. (gday -. truncate_float gday) in
  let hours, minutes, seconds = hms_of_decimal_hours ut in
  ((truncate_float gday, gm, gy), (hours, minutes, seconds))

let%test "ut_of_lct" =
  ut_of_lct (1., 7, 2013) (3., 37., 0.) 1. 4. = ((30., 6, 2013), (22., 37., 0.))

let lct_of_ut (day, month, year) hms daylight tzoffset =
  let ut = decimal_hours_of_hms hms in
  let zone_time = ut +. tzoffset in
  let local_time = zone_time +. daylight in
  let local_jd =
    julian_date_of_greenwich (day, month, year) +. (local_time /. 24.)
  in
  let gday, gmonth, gyear = greenwich_date_of_julian local_jd in
  let int_day = truncate_float gday in
  let lct = 24. *. (gday -. int_day) in
  ((int_day, gmonth, gyear), hms_of_decimal_hours lct)

let%test "lct_of_ut" =
  lct_of_ut (30., 6, 2013) (22., 37., 0.) 1. 4. = ((1., 7, 2013), (3., 37., 0.))
