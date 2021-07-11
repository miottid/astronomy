val string_of_month : int -> string
(* [string_of_month a] returns the month name. Months starts from index 1.*)

val date_of_easter : int -> int * int
(* [date_of_easter a] returns the date of Easter (day * month). *)

val greenwich_to_julian_date : float -> int -> int -> unit
(* [greenwich_to_julian_date day month year] returns the Julian date. *)