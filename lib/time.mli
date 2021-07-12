val string_of_month : int -> string
(* [string_of_month a] returns the month name. Months starts from index 1.*)

val date_of_easter : int -> int * int
(* [date_of_easter a] returns the date of Easter (day * month). *)

val greenwich_to_julian_date : float -> int -> int -> float
(* [greenwich_to_julian_date day month year] returns the Julian date. *)

val julian_to_greenwich_date : float -> float * int * int
(* [julian_to_greenwich_date julian] converts a julian date into greenwich. *)

val decimal_to_hms : float -> int * int * int
(* [decimal_to_hms hours] converts decimal hours to hours, minutes and seconds. *)
