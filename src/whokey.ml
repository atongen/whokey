type fingerprint =
  | Hex of string
  | Sha256 of string

type auth = {
  timestamp: float;
  host: string;
  port: int;
  fingerprint: fingerprint;
  comment: string
}

type status =
  | StillLoggedIn
  | LoggedOut of float

type last = {
  pts: string;
  timestamp: float;
  status: status;
}

let days = ["Sun", 0; "Mon", 1; "Tue", 2; "Wed", 3; "Thu", 4; "Fri", 5; "Sat", 6]
let days_ar = List.map fst days |> Array.of_list
let months =
  ["Jan", 0; "Feb", 1; "Mar", 2; "Apr", 3; "May",  4; "Jun",  5;
   "Jul", 6; "Aug", 7; "Sep", 8; "Oct", 9; "Nov", 10; "Dec", 11]
let months_ar = List.map fst months |> Array.of_list

let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false

let fingerprint_of_string str = if contains str "SHA256:" then Sha256 str else Hex str

let make_last_timestamp mon day hour min sec year =
  let mon = List.assoc mon months in
  fst (Unix.mktime {
      Unix.tm_sec=sec;
      tm_min=min;
      tm_hour=hour;
      tm_mon=mon;
      tm_mday=day;
      tm_year=year-1900;
      tm_wday=0;
      tm_yday=0;
      tm_isdst=false
    })

let make_auth_timestamp mon day hour min sec =
  let now = Unix.localtime (Unix.time ()) in
  let year = now.tm_year + 1900 in
  make_last_timestamp mon day hour min sec year

let parse_auth_line ~keys line =
  try
    Scanf.sscanf line "%s %d %d:%d:%d %s sshd[%d]: Accepted publickey for %s from %s port %d ssh2: RSA %s"
      (fun mon day hour min sec _ _ _ host port fingerprint ->
         let comment = match Hashtbl.find_opt keys fingerprint with
           | Some c -> c
           | None -> "[UNKNOWN]" in
         Some {
           timestamp = make_auth_timestamp mon day hour min sec;
           host;
           port;
           fingerprint = fingerprint_of_string fingerprint;
           comment;
         }
      )
  with _e -> None

let timestamp_from_last_tokens tokens =
  let ts = Str.split (Str.regexp ":") tokens.(2) |> Array.of_list in
  make_last_timestamp
    tokens.(0) (* mon *)
    (int_of_string tokens.(1)) (* day *)
    (int_of_string ts.(0)) (* hour *)
    (int_of_string ts.(1)) (* min *)
    (int_of_string ts.(2)) (* sec *)
    (int_of_string tokens.(3)) (* year *)

let parse_last_still_logged_in tokens =
  let pts = tokens.(1) in
  let login = timestamp_from_last_tokens (Array.sub tokens 3 4) in
  {
    pts;
    timestamp = login;
    status = StillLoggedIn;
  }

let parse_last_logged_out tokens =
  let pts = tokens.(1) in
  let login = timestamp_from_last_tokens (Array.sub tokens 3 4) in
  let logout = timestamp_from_last_tokens (Array.sub tokens 9 4) in
  {
    pts;
    timestamp = login;
    status = LoggedOut logout;
  }

(* https://stackoverflow.com/questions/39813584/how-to-split-on-whitespaces-in-ocaml *)
let split str = Str.split (Str.regexp "[ \n\r\x0c\t]+") str

let parse_last_line line whoami =
  if contains line whoami then
    let tokens = split line in
    if List.length tokens = 11 && contains line "still logged in" then
      Some (parse_last_still_logged_in (Array.of_list tokens))
    else if List.length tokens = 15 then
      Some (parse_last_logged_out (Array.of_list tokens))
    else None
  else None

let read_chan_filter_map chan f =
  let rec aux chan acc =
    try
      let line = input_line chan in
      match f line with
      | Some s -> aux chan (s :: acc)
      | None -> aux chan acc
    with End_of_file ->
      close_in chan;
      acc
  in
  List.rev @@ aux chan []

let read_process_line command =
  let ic = Unix.open_process_in command in
  let line = input_line ic in
  close_in ic;
  String.trim line

let read_process_filter_map command f =
  let chan = Unix.open_process_in command in
  read_chan_filter_map chan f

let read_file_filter_map path f =
  let chan = open_in path in
  read_chan_filter_map chan f

let read_files_filter_map paths f =
  let rec aux paths acc =
    match paths with
    | [] -> acc
    | hd :: tl -> if Sys.file_exists hd then
        let r = read_file_filter_map hd f in
        aux tl (List.concat [r; acc])
      else
        aux tl acc
  in
  aux (List.rev paths) []

let read_file_iter path f =
  let rec aux chan =
    try f @@ input_line chan; aux chan
    with End_of_file -> close_in chan
  in
  aux @@ open_in path

let handle_exc e = ignore(Printexc.to_string e |> print_endline)

(* http://caml.inria.fr/pub/ml-archives/caml-list/2003/07/5ff669a9d2be35ec585b536e2e0fc7ca.en.html *)
let protect ~f ~(finally: unit -> unit) =
  let result = ref None in
  try
    result := Some (f ());
    raise Exit
  with
    Exit as e ->
    finally ();
    (match !result with Some x -> x | None -> raise e)
  | e ->
    finally (); raise e

let build_fingerprint_table keys_path =
  let tbl = Hashtbl.create 10 in
  read_file_iter keys_path (fun line ->
      let name = Filename.temp_file "whokey-" ".tmp" in
      let descr = Unix.openfile name [Unix.O_RDWR] 0o600 in

      let out_channel = Unix.out_channel_of_descr descr in
      Printf.fprintf out_channel "%s\n" line;
      flush out_channel;

      let fingerprint_line = read_process_line (Printf.sprintf "ssh-keygen -lf %s" name) in
      protect ~f:(fun () ->
          Scanf.sscanf fingerprint_line "%d %s %s (RSA)"
            (fun _ fingerprint comment -> Hashtbl.add tbl fingerprint comment)
        ) ~finally:(fun () ->
          Unix.close descr;
          Sys.remove name;
          ()
        )
    ); tbl

let find_auth_for_last last auths =
  List.find_opt (fun (auth: auth) ->
      abs_float (auth.timestamp -. last.timestamp) <= 1.0
    ) auths

let format_time time =
  let tm = Unix.localtime time in
  Printf.sprintf "%s %s %2d %02d:%02d:%02d %04d"
    days_ar.(tm.tm_wday)
    months_ar.(tm.tm_mon)
    tm.tm_mday
    tm.tm_hour
    tm.tm_min
    tm.tm_sec
    (tm.tm_year + 1900)

let status_to_string = function
  | StillLoggedIn -> "still logged in         "
  | LoggedOut f -> format_time f

let last_auth_pair_to_string last auth =
  Printf.sprintf "%s - %s %s %s %s"
    (format_time last.timestamp)
    (status_to_string last.status)
    auth.comment
    last.pts
    auth.host

let process whoami =
  let auth_paths = ["/var/log/auth.log"; "/var/log/auth.log.1"] in
  let keys_path = Printf.sprintf "/home/%s/.ssh/authorized_keys" whoami in
  let keys = build_fingerprint_table keys_path in

  let auth_match = Printf.sprintf "Accepted publickey for %s" whoami in
  let auth_list = read_files_filter_map auth_paths (fun line ->
      if contains line auth_match then parse_auth_line ~keys line
      else None
    ) in

  let last_cmd = Printf.sprintf "last -Fa %s" whoami in
  read_process_filter_map last_cmd (fun line ->
      match parse_last_line line whoami with
      | Some last -> (
          match find_auth_for_last last auth_list with
          | Some auth -> Some (last, auth)
          | None -> None)
      | None -> None)

let usage = {eousage|
Usage: whokey [USER]
Print login information from `last` command, including key information from auth.log
|eousage}

let () =
  let n = Array.length Sys.argv in
  if (n = 2 && List.mem Sys.argv.(1) ["-h"; "--help"]) || n > 2 then
    print_string usage
  else
    let whoami = if n = 2
      then Sys.argv.(1)
      else read_process_line "whoami"
    in
    try
      process whoami |> List.iter (fun (last, auth) ->
          print_endline (last_auth_pair_to_string last auth))
    with e -> handle_exc e; print_string usage
