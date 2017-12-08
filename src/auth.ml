type t = {
  user: string;
  timestamp: Time_parser.t;
  fingerprint: string;
}

let of_string str =
  let tokens = split str in
  {
    user = List.nth tokens 8;
    timestamp = Util.sublist_str tokens 0 3 |> Time_parser.of_auth;
    fingerprint = List.nth tokens 15;
  }

let to_string { user; timestamp; fingerprint; } =
  Printf.sprintf "%s %s %s" (Time_parser.epoch timestamp) user fingerprint
