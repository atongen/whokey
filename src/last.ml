open Core.Std

type t = {
  user: string;
  timestamp: Time;
  pts: string;
}

let of_string str =
  let tokens = Util.split str in
  {
    user = List.nth tokens 0;
    timestamp = Date_Parser.of_last (Util.sublist_str tokens 3 5);
    pts = List.nth tokens 1;
  }

let to_string { user; timestamp; pts } =
  Printf.sprintf "%s %s %s" (Time.to_sec_string timestamp) user pts
