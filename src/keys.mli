type t

val from_file : string -> t
val from_alist : (string * string) list -> t

val find : t -> string -> string option
