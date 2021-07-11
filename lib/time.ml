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
  let a = year mod 19 in
  let b = year / 100 in
  let c = year mod 100 in
  let d = b / 4 in
  let e = b mod 4 in
  let f = (b + 8) / 25 in
  let g = (b - f + 1) / 3 in
  let h = ((19 * a) + b - d - g + 15) mod 30 in
  let i = c / 4 in
  let k = c mod 4 in
  let l = (32 + (2 * e) + (2 * i) - h - k) mod 7 in
  let m = (a + (11 * h) + (22 * l)) / 451 in
  let month_number = (h + l - (7 * m) + 114) / 31 in
  let day_of_month = ((h + l - (7 * m) + 114) mod 31) + 1 in
  (day_of_month, month_number)

let%test "date_of_easter 2019" = date_of_easter 2019 = (21, 4)

let%test "date_of_easter 2020" = date_of_easter 2020 = (12, 4)

let%test "date_of_easter 2021" = date_of_easter 2021 = (4, 4)

let%test "date_of_easter 2022" = date_of_easter 2022 = (17, 4)

let%test "date_of_easter 2023" = date_of_easter 2023 = (9, 4)

let%test "date_of_easter 2024" = date_of_easter 2024 = (31, 3)

let%test "date_of_easter 2025" = date_of_easter 2025 = (20, 4)
