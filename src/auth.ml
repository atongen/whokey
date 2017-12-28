(* example:
 * Dec 26 14:27:15 drip-staging-ansible sshd[26413]: Accepted publickey for ubuntu from 216.70.43.154 port 45300 ssh2: RSA SHA256:auz5n5RrA1X0IFXvaZr7VEuR/WpsA9FIOPS1yGtaRzI
 *)
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
