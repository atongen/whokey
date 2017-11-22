(*
 * Oct 27 17:53:03 drip-production-ansible sshd[14313]: Accepted publickey for ubuntu from 216.70.43.154 port 32876 ssh2: RSA 37:c9:85:f8:7d:b7:b8:da:6a:47:3e:ea:97:05:9c:ce
 *)

type t = {
  user: string;
  timestamp: string;
  fingerprint: string;
}

let of_string str =
  let tokens = split str in
  {
    user = List.nth tokens 8;
    timestamp = Util.sublist_str tokens 0 3;
    fingerprint = List.nth tokens 15;
  }

let to_string { user; timestamp; fingerprint; } =
  Printf.sprintf "%s %s %s" timestamp user fingerprint
