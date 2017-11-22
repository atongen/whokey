(*
 * ubuntu   pts/5        Mon Oct  2 15:18:21 2017 - Mon Oct  2 15:19:41 2017  (00:01)
 * or
 * ubuntu   pts/0        Sat Oct 28 14:21:11 2017   still logged in
 * or
 * ubuntu   tty7         Mon Oct  2 08:14:16 2017 - crash                     (23:51)
 *)

type t = {
  user: string;
  timestamp: string;
  pts: string;
}

let of_string str =
  let tokens = Util.split str in
  {
    user = List.nth tokens 0;
    timestamp = Util.sublist_str tokens 3 3;
    pts = List.nth tokens 1;
  }

let to_string { user; timestamp; pts } =
  Printf.sprintf "%s %s %s" timestamp user pts
