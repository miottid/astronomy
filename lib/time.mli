val string_of_month : int -> string
(* [string_of_month a] returns the month name. Months starts from index 1.*)

val date_of_easter : int -> int * int
(* [date_of_easter a] returns the date of Easter (day * month). *)

val julian_of_greenwich : float -> int -> int -> float
(* [julian_of_greenwich day month year] returns the Julian date. *)

val greenwich_of_julian : float -> float * int * int
(* [greenwich_of_julian julian] converts a julian date into greenwich. *)

val hms_of_decimal : float -> int * int * int
(* [hms_of_decimal hours] converts decimal hours to hours, minutes and seconds. *)

val decimal_of_hms : float -> float -> float -> float
(* [decimal_of_hms hours minutes seconds] converts hms to decimal hours. *)