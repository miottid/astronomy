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

let%test "date_of_easter 2019" = date_of_easter 2019 = (21, 4)

let%test "date_of_easter 2020" = date_of_easter 2020 = (12, 4)

let%test "date_of_easter 2021" = date_of_easter 2021 = (4, 4)

let%test "date_of_easter 2022" = date_of_easter 2022 = (17, 4)

let%test "date_of_easter 2023" = date_of_easter 2023 = (9, 4)

let%test "date_of_easter 2024" = date_of_easter 2024 = (31, 3)

let%test "date_of_easter 2025" = date_of_easter 2025 = (20, 4)

let truncate_float f = float_of_int (truncate f)

let greenwich_to_julian_date day month year =
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

let%test "greenwich_to_julian_date#1" =
  greenwich_to_julian_date 19.75 6 2009 = 2455002.25

let%test "greenwich_to_julian_date#2" =
  greenwich_to_julian_date 12.625 7 2021 = 2459408.125

let julian_to_greenwich_date julian =
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

let%test "julian_to_greenwich_date" =
  julian_to_greenwich_date 2455002.25 = (19.75, 6, 2009)

let decimal_to_hms hours =
  let unsigned_decimal = Float.abs hours in
  let total_seconds = truncate (unsigned_decimal *. 3600.) in
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
  (signed_hours, minutes, corrected_seconds)

let%test "decimal_to_hms" = decimal_to_hms 18.52416667 = (18, 31, 27)
