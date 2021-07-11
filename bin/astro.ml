open Lib

let () =
  let day, month = Time.date_of_easter 2025 in
  Printf.printf "Date of Easter: %d, %s.\n" day (Time.string_of_month month)
