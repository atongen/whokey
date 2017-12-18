type t = {
  user: string;
  timestamp: Time_parser.t;
  pts: string;
}

let of_string str =
  let tokens = Util.split str in
  {
    user = List.nth tokens 0;
    timestamp = Util.sublist_str tokens 3 5 |> Time_parser.of_last;
    pts = List.nth tokens 1;
  }

let to_string { user; timestamp; pts } =
  Printf.sprintf "%.0f %s %s" (Time_parser.epoch timestamp) user pts
