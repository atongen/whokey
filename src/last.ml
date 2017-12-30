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
  Printf.sprintf "%s %s %s" (Time_parser.pretty timestamp) user pts

let is_auth {user=lu; timestamp=lt; _}
            {Auth.
              user=au; timestamp=at; _} =
  (lu = au) && (Time_parser.same lt at)

let find_auths last auths =
  let is_my_auth auth = is_auth last auth in
  List.filter is_my_auth auths