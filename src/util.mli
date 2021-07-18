val approx_equal : float -> float -> bool
(** [approx_equal a b] returns true if [a] > ([b] - epsilon) and [a] < ([b] + epsilon). *)

val roundn : float -> int -> float
(** [roundn value dp] round numbers to [dp] decimal places. *)