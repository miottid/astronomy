val approx_equal : float -> float -> bool
(** [approx_equal a b] 
    returns true if [a] > ([b] - epsilon) and [a] < ([b] + epsilon). *)

val roundn : float -> int -> float
(** [roundn value dp] round numbers to [dp] decimal places. *)

val radians_of_degrees : float -> float
(** [radians_of_degrees radians] converts radians to degrees. *)

val degrees_of_radians : float -> float
(** [degrees_of_radians degrees] converts degrees to radians. *)

val degrees_of_hours : float -> float
(** [degrees_of_hours hours] converts decimal hours to decimal degrees. *)

val hours_of_degrees : float -> float
(** [hours_of_degrees degrees] converts decimal degrees to decimal hours. *)

val truncate_f : float -> float
(** [truncate_f] does the same as [truncate], but returns a float. *)