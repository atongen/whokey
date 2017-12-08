type t

val diff : t -> t -> float

val same : t -> t -> bool

val of_last : string -> t

val of_auth : string -> t

val epoch : t -> float
