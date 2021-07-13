type date = float * int * int
(* date represent a date with day, month and year *)

type hms = float * float * float
(* hms is a tuple of hours, minutes and seconds. *)

val string_of_month : int -> string
(* [string_of_month a] returns the month name. Months starts from index 1.*)

val string_of_weekday : int -> string
(* [string_of_weekday weekday] returns the day of the week *)

val date_of_easter : int -> int * int
(* [date_of_easter a] returns the date of Easter (day * month). *)

val julian_of_greenwich : date -> float
(* [julian_of_greenwich day month year] returns the Julian date. *)

val greenwich_of_julian : float -> date
(* [greenwich_of_julian julian] converts a julian date into greenwich. *)

val hms_of_decimal : float -> hms
(* [hms_of_decimal hours] converts decimal hours to hours, minutes and seconds. *)

val decimal_of_hms : hms -> float
(* [decimal_of_hms hours minutes seconds] converts hms to decimal hours. *)

val weekday_of_julian_date : float -> int
(* [weekday_of_julian_date jd] returns the weekday number at Greenwich. Sunday = 0. *)

val weekday_of_date : date -> int
(* [weekday_of_date date] returns the weekday number at Greenwich. Sunday = 0. *)