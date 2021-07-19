type date = { day : float; month : int; year : int }

type time = { hours : float; minutes : float; seconds : float }

type datetime = { date : date; time : time }

type datetime_tz = { tzoffset : float; daylight : float; datetime : datetime }

let pp_time time =
  Printf.sprintf "%fh %fm %fs" time.hours time.minutes time.seconds

let pp_date date = Printf.sprintf "%.5f/%d/%d" date.day date.month date.year

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
  let a = Float.floor (float yd /. 100.) in
  let b =
    if
      yd > 1582
      || (yd = 1582 && date.month > 10)
      || (yd = 1582 && date.month = 10 && date.day > 15.)
    then 2. -. a +. Float.floor (a /. 4.)
    else 0.
  in
  let c =
    if yd < 0 then Float.floor ((365.25 *. float yd) -. 0.75)
    else Float.floor (365.25 *. float yd)
  and d = Float.floor (30.6001 *. (float md +. 1.)) in
  b +. c +. d +. date.day +. 1720994.5

let greenwich_of_julian julian =
  let i = Float.floor (julian +. 0.5) in
  let f = julian +. 0.5 -. i in
  let b =
    if i > 2299160. then
      let a = Float.floor ((i -. 1867216.25) /. 36524.25) in
      i +. 1. +. a -. Float.floor (a /. 4.)
    else i
  in
  let c = b +. 1524. in
  let d = Float.floor ((c -. 122.1) /. 365.25) in
  let e = Float.floor (365.25 *. d) in
  let g = Float.floor ((c -. e) /. 30.6001) in
  let day = c -. e +. f -. Float.floor (30.6001 *. g) in
  let month = if g < 13.5 then g -. 1. else g -. 13. in
  let year = if month > 2.5 then d -. 4716. else d -. 4715. in
  { day; month = truncate month; year = int_of_float year }

let time_of_hours hours =
  let unsigned_decimanl = Float.abs hours in
  let total_seconds = unsigned_decimanl *. 3600. in
  let seconds = Util.roundn (mod_float total_seconds 60.) 2 in
  let corrected_seconds =
    if Util.approx_equal seconds 60. then 0. else seconds
  in
  let corrected_remainder =
    if Util.approx_equal seconds 60. then total_seconds +. 60.
    else total_seconds
  in
  let minutes = mod_float (Float.floor (corrected_remainder /. 60.)) 60. in
  let unsigned_hours = Float.floor (corrected_remainder /. 3600.) in
  let signed_hours = if hours < 0. then ~-.unsigned_hours else unsigned_hours in
  { hours = signed_hours; minutes; seconds = corrected_seconds }

let hours_of_time time =
  let a = Float.abs time.seconds /. 60. in
  let b = (Float.abs time.minutes +. a) /. 60. in
  let c = Float.abs time.hours +. b in
  if time.hours < 0. || time.minutes < 0. || time.seconds < 0. then ~-.c else c

let weekday_of_julian julian =
  let jd = Float.floor (julian -. 0.5) +. 0.5 in
  truncate (jd +. 1.5) mod 7

let weekday_of_date date = weekday_of_julian (julian_of_greenwich date)

let ut_of_lct lct =
  let lct_hours = hours_of_time lct.datetime.time in
  let ut = lct_hours -. lct.daylight -. lct.tzoffset in
  let gday = lct.datetime.date.day +. (ut /. 24.) in
  let gdate =
    {
      day = gday;
      month = lct.datetime.date.month;
      year = lct.datetime.date.year;
    }
  in
  let jd = julian_of_greenwich gdate in
  let gdate = greenwich_of_julian jd in
  let rest, _ = modf gdate.day in
  let ut = 24. *. rest in
  let time = time_of_hours ut in
  {
    date =
      { day = Float.floor gdate.day; month = gdate.month; year = gdate.year };
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
  let int_day = Float.floor gdate.day in
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
  let t0 = t0 -. (24. *. Float.floor (t0 /. 24.)) in
  let ut = hours_of_time datetime.time in
  let a = ut *. 1.002737909 in
  let gst = t0 +. a in
  let gst = gst -. (24. *. Float.floor (gst /. 24.)) in
  time_of_hours gst

let ut_of_gst datetime =
  let jd = julian_of_greenwich datetime.date in
  let s = jd -. 2451545. in
  let t = s /. 36525. in
  let t0 = 6.697374558 +. (2400.051336 *. t) +. (0.000025862 *. t *. t) in
  let t0 = t0 -. (24. *. Float.floor (t0 /. 24.)) in
  let gsthrs = hours_of_time datetime.time in
  let a = gsthrs -. t0 in
  let b = a -. (24. *. Float.floor (a /. 24.)) in
  let ut = b *. 0.9972695663 in
  time_of_hours ut

let lst_of_gst (time, geog_long_deg) =
  let gst = hours_of_time time in
  let offset = geog_long_deg /. 15. in
  let lst_hours = gst +. offset in
  let lst_hours = lst_hours -. (24. *. Float.floor (lst_hours /. 24.)) in
  time_of_hours lst_hours

let gst_of_lst (time, geog_long_deg) =
  let gst = hours_of_time time in
  let offset = geog_long_deg /. 15. in
  let gst = gst -. offset in
  let gst = gst -. (24. *. Float.floor (gst /. 24.)) in
  time_of_hours gst
