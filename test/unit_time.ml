open Astronomy

let approx_equal a b = Stdlib.(abs_float (a -. b) < epsilon_float)

let validate_results f lst =
  List.fold_left (fun acc (input, output) -> acc && f input = output) true lst

module Time_test = struct
  let easter_day () =
    validate_results Time.easter_day
      [
        (2019, { day = 21.; month = 4; year = 2019 });
        (2020, { day = 12.; month = 4; year = 2020 });
        (2021, { day = 4.; month = 4; year = 2021 });
        (2022, { day = 17.; month = 4; year = 2022 });
        (2023, { day = 9.; month = 4; year = 2023 });
        (2024, { day = 31.; month = 3; year = 2024 });
        (2025, { day = 20.; month = 4; year = 2025 });
      ]

  let julian_of_greenwich () =
    validate_results Time.julian_of_greenwich
      [
        ({ day = 19.75; month = 6; year = 2009 }, 2455002.25);
        ({ day = 12.625; month = 7; year = 2021 }, 2459408.125);
      ]

  let greenwich_of_julian () =
    Time.greenwich_of_julian 2455002.25
    = { day = 19.75; month = 6; year = 2009 }

  let time_of_hours () =
    validate_results Time.time_of_hours
      [
        (18.5, { hours = 18.; minutes = 30.; seconds = 0. });
        (22.5, { hours = 22.; minutes = 30.; seconds = 0. });
      ]

  let hours_of_time () =
    validate_results Time.hours_of_time
      [ ({ hours = 18.; minutes = 30.; seconds = 0. }, 18.5) ]
end

let easter_day () =
  Alcotest.(check bool) "easter_day" true (Time_test.easter_day ())

let julian_of_greenwich () =
  Alcotest.(check bool)
    "julian_of_greenwich" true
    (Time_test.julian_of_greenwich ())

let greenwich_of_julian () =
  Alcotest.(check bool)
    "greenwich_of_julian" true
    (Time_test.greenwich_of_julian ())

let time_of_hours () =
  Alcotest.(check bool) "time_of_hours" true (Time_test.time_of_hours ())

let hours_of_time () =
  Alcotest.(check bool) "hours_of_time" true (Time_test.hours_of_time ())

let test_set =
  [
    ("easter_day", `Quick, easter_day);
    ("julian_of_greenwich", `Quick, julian_of_greenwich);
    ("greenwich_of_julian", `Quick, greenwich_of_julian);
    ("time_of_hours", `Quick, time_of_hours);
    ("hours_of_time", `Quick, hours_of_time);
  ]
