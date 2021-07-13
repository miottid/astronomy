(* date represent a date with day, month and year *)
type date = float * int * int

(* hms is a tuple of hours, minutes and seconds. *)
type hms = float * float * float

(* [string_of_month a] returns the month name.
 * Months starts from index 1. *)
val string_of_month : int -> string

(* [string_of_weekday weekday] returns the day of the week *)
val string_of_weekday : int -> string

(* [date_of_easter a] returns the date of Easter (day * month). *)
val date_of_easter : int -> int * int

(* [julian_date_of_greenwich day month year] returns the Julian date. *)
val julian_date_of_greenwich : date -> float

(* [greenwich_date_of_julian julian] converts a julian date into greenwih. *)
val greenwich_date_of_julian : float -> date

(* [hms_of_decimal_hours hours] converts decimal hours to 
 * hours, minutes and seconds. *)
val hms_of_decimal_hours : float -> hms

(* [decimal_of_hms hours minutes seconds] converts hms to decimal hours. *)
val decimal_hours_of_hms : hms -> float

(* [weekday_of_julian_date jd] returns the weekday number at Greenwich.
 * With Sunday = 0. *)
val weekday_of_julian_date : float -> int

(* [weekday_of_date date] returns the weekday number at Greenwich.
 * Sunday = 0. *)
val weekday_of_date : date -> int