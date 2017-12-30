type t = {
  user: string;
  timestamp: Time_parser.t;
  fingerprint: string;
}

let of_string str =
  let tokens = Util.split str in
  {
    user = List.nth tokens 8;
    timestamp = Util.sublist_str tokens 0 3 |> Time_parser.of_auth;
    fingerprint = List.nth tokens 15;
  }

let to_string { user; timestamp; fingerprint; } =
  Printf.sprintf "%s %s %s" (Time_parser.pretty timestamp) user fingerprint

let compare a1 a2 =
  compare (Time_parser.epoch a1.timestamp) (Time_parser.epoch a2.timestamp)