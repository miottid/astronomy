open Astronomy

let approx_equal a b = Float.abs (a -. b) < epsilon_float

let approx_time_equal (t1 : Time.time) (t2 : Time.time) =
  Float.abs (t1.hours -. t2.hours) < epsilon_float
  && Float.abs (t1.minutes -. t2.minutes) < epsilon_float
  && Float.abs (t1.seconds -. t2.seconds) < epsilon_float

let approx_date_equal (d1 : Time.date) (d2 : Time.date) =
  Float.abs (d1.day -. d2.day) < epsilon_float
  && d1.month = d2.month && d1.year = d2.year

let approx_datetime_equal (dt1 : Time.datetime) (dt2 : Time.datetime) =
  approx_date_equal dt1.date dt2.date && approx_time_equal dt1.time dt2.time

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
    validate_results Time.greenwich_of_julian
      [
        (2455002.25, { day = 19.75; month = 6; year = 2009 });
        (2459408.125, { day = 12.625; month = 7; year = 2021 });
      ]

  let time_of_hours () =
    validate_results Time.time_of_hours
      [
        (18.5, { hours = 18.; minutes = 30.; seconds = 0. });
        (22.5, { hours = 22.; minutes = 30.; seconds = 0. });
      ]

  let hours_of_time () =
    validate_results Time.hours_of_time
      [ ({ hours = 18.; minutes = 30.; seconds = 0. }, 18.5) ]

  let weekday_of_julian () = Time.weekday_of_julian 2455001.5 = 5

  let weekday_of_date () =
    Time.weekday_of_date { day = 19.; month = 6; year = 2009 } = 5

  let ut_of_lct () =
    let ut =
      Time.ut_of_lct
        {
          datetime =
            {
              date = { day = 1.; month = 7; year = 2013 };
              time = { hours = 3.; minutes = 37.; seconds = 0. };
            };
          tzoffset = 4.;
          daylight = 1.;
        }
    in
    approx_datetime_equal ut
      {
        date = { day = 30.; month = 6; year = 2013 };
        time = { hours = 22.; minutes = 37.; seconds = 0. };
      }

  let lct_of_ut () =
    Time.lct_of_ut
      {
        datetime =
          {
            date = { day = 30.; month = 6; year = 2013 };
            time = { hours = 22.; minutes = 37.; seconds = 0. };
          };
        tzoffset = 4.;
        daylight = 1.;
      }
    = {
        date = { day = 1.; month = 7; year = 2013 };
        time = { hours = 3.; minutes = 37.; seconds = 0. };
      }

  let gst_of_ut () =
    approx_time_equal
      (Time.gst_of_ut
         {
           date = { day = 22.; month = 4; year = 1980 };
           time = { hours = 14.; minutes = 36.; seconds = 51.67 };
         })
      { hours = 4.; minutes = 40.; seconds = 5. }
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

let weekday_of_julian () =
  Alcotest.(check bool)
    "weekday_of_julian" true
    (Time_test.weekday_of_julian ())

let weekday_of_date () =
  Alcotest.(check bool) "weekday_of_date" true (Time_test.weekday_of_date ())

let ut_of_lct () =
  Alcotest.(check bool) "ut_of_lct" true (Time_test.ut_of_lct ())

let lct_of_ut () =
  Alcotest.(check bool) "lct_of_ut" true (Time_test.lct_of_ut ())

let gst_of_ut () =
  Alcotest.(check bool) "gst_of_ut" true (Time_test.gst_of_ut ())

let test_set =
  [
    ("easter_day", `Quick, easter_day);
    ("julian_of_greenwich", `Quick, julian_of_greenwich);
    ("greenwich_of_julian", `Quick, greenwich_of_julian);
    ("time_of_hours", `Quick, time_of_hours);
    ("hours_of_time", `Quick, hours_of_time);
    ("weekday_of_julian", `Quick, weekday_of_julian);
    ("weekday_of_date", `Quick, weekday_of_date);
    ("ut_of_lct", `Quick, ut_of_lct);
    ("lct_of_ut", `Quick, lct_of_ut);
    ("gst_of_ut", `Quick, gst_of_ut);
  ]
