val approx_equal : float -> float -> bool
(** [approx_equal a b] 
    returns true if [a] > ([b] - epsilon) and [a] < ([b] + epsilon). *)

val roundn : float -> int -> float
(** [roundn value dp] round numbers to [dp] decimal places. *)

val radians_of_degrees : float -> float
(** [radians_of_degrees radians] converts radians to degrees. *)

val degrees_of_radians : float -> float
(** [degrees_of_radians degrees] converts degrees to radians. *)

val deg_of_ha : float -> float
(** [deg_of_ha hours] converts decimal hours to decimal degrees. *)

val ha_of_deg : float -> float
(** [ha_of_deg degrees] converts decimal degrees to decimal hours. *)
