let () =
  Alcotest.run "Astronomy"
    [
      ("Time", Unit_timescale.test_set);
      ("Coordinate_system", Unit_coordinate_system.test_set);
    ]
