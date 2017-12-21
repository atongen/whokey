type t = {
  user: string;
  timestamp: Time_parser.t;
  fingerprint: string;
}

val of_string : string -> t
val to_string : t -> string
