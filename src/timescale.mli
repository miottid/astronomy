(** Perform date and time calculations *)

type date = { day: float; month: int; year: int }
(** [date] represent a particular day with time. *)

type time = { hours: float; minutes: float; seconds: float }
(** [time] is represent a specific time in hours, minutes and seconds. *)

type datetime = { date: date; time: time }
(** [datetime] is a date with time. *)

type datetime_tz = { tzoffset: float; daylight: float; datetime: datetime }
(** [datetime_tz] is a date with time, daylight saving and time zone offset.
    [daylight] and [tzoffset] are expressed in hours. *)

val pp_time : time -> string
(** [pp_time time] converts a [time] type to a human readable string.
    e.g.: `2.5h 15.0m 25.0s`. *)

val pp_date : date -> string
(** [pp_date date] converts [date] to a human readable string. 
    e.g: `19.5/6/2021`. *)

val string_of_month : int -> string
(** [string_of_month a] returns the month name.
    Months starts from index 1. *)

val string_of_weekday : int -> string
(** [string_of_weekday weekday] returns the integer day of the week.
    0 is Sunday. *)

val easter_day : int -> date
(** [easter_day a] returns the date of Easter in the provided [year]. *)

val julian_of_greenwich : date -> float
(** [julian_of_greenwich day month year] returns the Julian date. *)

val greenwich_of_julian : float -> date
(** [greenwich_of_julian julian] converts a Julian date 
    to a greenwich date. *)

val time_of_hours : float -> time
(** [time_of_hours hours] converts decimal hours to 
    hours, minutes and seconds. *)

val hours_of_time : time -> float
(** [hours_of_time hours minutes seconds] converts time to 
    decimal hours. *)

val weekday_of_julian : float -> int
(** [weekday_of_julian jd] returns the weekday number at Greenwich.
    With 'Sunday' = 0. *)

val weekday_of_date : date -> int
(** [weekday_of_date date] returns the weekday number at Greenwich.
    With 'Sunday' = 0. *)

val ut_of_lct : datetime_tz -> datetime
(** [ut_of_lct datetime_tz] converts Local Civil Time to 
    Universal Time. *)

val lct_of_ut : datetime_tz -> datetime
(** [lct_of_ut datetime_tz] converts Universal Time to 
    Local Civil Time. *)

val gst_of_ut : datetime -> time
(** [gst_of_ut (date, time)] converts Universal time to 
    Greenwich Sideral Time. *)

val ut_of_gst : datetime -> time
(** [ut_of_gst (date, time)] converts Greenwich Sideral Time to 
    Universal Time. *)

val lst_of_gst : time * float -> time
(** [lst_of_gst time geog_long_deg] 
    converts Greenwich Sideral Time to Local Sideral Time.*)

val gst_of_lst : time * float -> time
(** [gst_of_lst time geog_long_deg] 
    converts Local Sideral Time to Greenwich Sideral Time.*)