type t

val of_string : string -> t
val to_string : t -> string

val is_auth : t -> Auth.t -> bool
val find_auths : t -> Auth.t list -> Auth.t list
