type date = float * int * int
(** date represent a date with day, month and year. *)

type hms = float * float * float
(** hms is a tuple of hours, minutes and seconds. *)

val string_of_month : int -> string
(** [string_of_month a] returns the month name.
    Months starts from index 1. *)

val string_of_weekday : int -> string
(** [string_of_weekday weekday] returns the day of the week.
    0 is Sunday. *)

val date_of_easter : int -> int * int
(** [date_of_easter a] returns the date of Easter (day * month). *)

val julian_date_of_greenwich : date -> float
(** [julian_date_of_greenwich day month year] returns the Julian date. *)

val greenwich_date_of_julian : float -> date
(** [greenwich_date_of_julian julian] converts a julian date into greenwih. *)

val hms_of_decimal_hours : float -> hms
(** [hms_of_decimal_hours hours] converts decimal hours to 
    hours, minutes and seconds. *)

val decimal_hours_of_hms : hms -> float
(** [decimal_of_hms hours minutes seconds] converts hms to decimal hours. *)

val weekday_of_julian_date : float -> int
(** [weekday_of_julian_date jd] returns the weekday number at Greenwich.
    With Sunday = 0. *)

val weekday_of_date : date -> int
(** [weekday_of_date date] returns the weekday number at Greenwich.
    Sunday = 0. *)

val ut_of_lct : date -> hms -> float -> float -> date * hms
(** [ut_of_lct (day, month, year) (hrs, mins, secs) daylight tzoffset]
    Converts Local Civil Time to Universal Time. *)

val lct_of_ut : date -> hms -> float -> float -> date * hms
(** [lct_of_ut (day, month, year) (hrs, mins, secs) daylight tzoffset]
    Converts Universal Time to Local Civil Time. *)

val gst_of_ut : date -> hms -> hms
(** [gst_of_ut date hms] Converts Universal time to Greenwich Sideral Time. *)