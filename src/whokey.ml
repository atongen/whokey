open Lwt.Infix

type fingerprint =
    | Hex of string
    | Sha256 of string

type key = {
    bytes: int;
    fingerprint: fingerprint;
    comment: string;
}

type auth = {
    timestamp: float;
    host: string;
    port: int;
    fingerprint: fingerprint;
}

type auth_key = {
    timestamp: float;
    comment: string;
    host: string;
}

type status =
    | StillLoggedIn
    | LoggedOut of float

type last = {
    pts: string;
    timestamp: float;
    status: status;
    host: string;
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

let fingerprint_to_string = function
    | Hex s -> Printf.sprintf "HEX:%s" s
    | Sha256 s -> s

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
    make_last_timestamp mon day hour min sec now.tm_year

(* Feb 23 19:28:03 drip-production-ansible sshd[27871]: Accepted publickey for app from 73.71.195.74 port 38547 ssh2: RSA 45:53:65:5c:2c:fb:21:f3:bf:fc:e7:93:15:60:a4:5a *)
let parse_auth_line line =
    Scanf.sscanf line "%s %d %d:%d:%d %s sshd[%d]: Accepted publickey for %s from %s port %d ssh2: RSA %s"
    (fun mon day hour min sec _ _ _ host port fingerprint ->
        {
            timestamp = make_auth_timestamp mon day hour min sec;
            host;
            port;
            fingerprint = fingerprint_of_string fingerprint;
        }
    )

let auth_to_string (auth: auth) =
    Printf.sprintf "%f %s %d %s" auth.timestamp auth.host auth.port (fingerprint_to_string auth.fingerprint)

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
    let host = tokens.(10) in
    {
        pts;
        timestamp = login;
        status = StillLoggedIn;
        host;
    }

let parse_last_logged_out tokens =
    let pts = tokens.(1) in
    let login = timestamp_from_last_tokens (Array.sub tokens 3 4) in
    let logout = timestamp_from_last_tokens (Array.sub tokens 9 4) in
    let host = tokens.(14) in
    {
        pts;
        timestamp = login;
        status = LoggedOut logout;
        host;
    }

(* https://stackoverflow.com/questions/39813584/how-to-split-on-whitespaces-in-ocaml *)
let split str = Str.split (Str.regexp "[ \n\r\x0c\t]+") str


(**
 * ubuntu   pts/2        Sat Feb 23 16:00:04 2019   still logged in                       c-174-52-112-147.hsd1.ut.comcast.net
 * ubuntu   pts/1        Sat Feb 23 15:46:00 2019 - Sat Feb 23 16:05:51 2019  (00:19)     c-73-71-195-74.hsd1.ca.comcast.net
 *)
let parse_last_line line whoami =
    if contains line whoami then
        let tokens = split line in
        if List.length tokens = 11 && contains line "still logged in" then
            Some (parse_last_still_logged_in (Array.of_list tokens))
        else if List.length tokens = 15 then
            Some (parse_last_logged_out (Array.of_list tokens))
        else None
    else None

let compact l =
    let rec aux acc = function
    | [] -> acc
    | hd :: tl -> match hd with
        | Some x -> aux (x :: acc) tl
        | None -> aux acc tl
    in
    aux [] (List.rev l)

let zip_auths_keys (auths: auth list) (keys: key list) =
    List.map (fun auth ->
        let maybe_key = List.find_opt (fun (k: key) -> auth.fingerprint = k.fingerprint) keys in
        match maybe_key with
        | Some key -> Some { timestamp = auth.timestamp; comment = key.comment; host = auth.host }
        | None -> None
    ) auths
    |> compact

(* synchronous *)
let read_process command =
    let buffer_size = 2048 in
    let buffer = Buffer.create buffer_size in
    let string = Bytes.create buffer_size in
    let in_channel = Unix.open_process_in command in
    let chars_read = ref 1 in
    while !chars_read <> 0 do
        chars_read := input in_channel string 0 buffer_size;
        Buffer.add_substring buffer (Bytes.to_string string) 0 !chars_read
    done;
    ignore (Unix.close_process_in in_channel);
    Buffer.contents buffer

let parse_key_line path =
    let fingerprint_line = read_process (Printf.sprintf "ssh-keygen -lf %s" path) in
    Scanf.sscanf fingerprint_line "%d %s %s (RSA)"
    (fun bytes fingerprint comment ->
        {
            bytes;
            fingerprint = fingerprint_of_string fingerprint;
            comment;
        }
    )

let find_last_auth_key last auth_keys =
    let maybe_auth_key = List.find_opt (fun (ak: auth_key) ->
        abs_float (ak.timestamp -. last.timestamp) < 2.0 &&
            String.equal ak.host last.host
    ) auth_keys in
    match maybe_auth_key with
    | Some auth_key -> Some (last, auth_key)
    | None -> None

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
    | StillLoggedIn -> "still logged in"
    | LoggedOut f -> format_time f

let print_last_auth_key_pairs (last, auth_key) =
    Lwt_io.printf "%s %s - %s %s %s\n"
        auth_key.comment
        (format_time last.timestamp)
        (status_to_string last.status)
        last.pts
        last.host

let go whoami keys_path auth_path =
    (* keys *)
    let keys_stream = Lwt_process.pread_lines ("", [|"cat"; keys_path|]) in
    let keys = Lwt_stream.map (fun line ->
        (* Open a temporary file for reading and writing. *)
        let name = Filename.temp_file "whokey-" ".tmp" in
        let descr = Unix.openfile name [Unix.O_RDWR] 0o600 in

        (* Write ten lines of output. *)
        let out_channel = Unix.out_channel_of_descr descr in
        Printf.fprintf out_channel "%s\n" line;
        flush out_channel;

        let key = parse_key_line name in

        (* Close the underlying file descriptor and remove the file. *)
        Unix.close descr;
        Sys.remove name;

        key
    ) keys_stream
    |> Lwt_stream.to_list in

    (* auth *)
    let auth_match = Printf.sprintf "Accepted publickey for %s" whoami in
    let auth_stream = Lwt_process.pread_lines ("", [|"cat"; auth_path|]) in
    let auths = Lwt_stream.filter_map (fun line ->
        if contains line auth_match then
            Some (parse_auth_line line)
        else None
    ) auth_stream
    |> Lwt_stream.to_list in

    (* last *)
    keys >>= fun key_list ->
    auths >>= fun auth_list ->
    let auth_keys = zip_auths_keys auth_list key_list in
    let last_stream = Lwt_process.pread_lines ("", [|"last"; "-Fad"; whoami|]) in
    let lasts = Lwt_stream.filter_map (fun line ->
        let maybe_last = parse_last_line line whoami in
        match maybe_last with
        | Some last -> find_last_auth_key last auth_keys
        | None -> None
    ) last_stream in

    Lwt_stream.iter_p print_last_auth_key_pairs lasts

let () =
    (*let whoami = read_process "whoami" in*)
    let whoami = "ubuntu" in

    (*let keys_path = Printf.sprintf "/home/%s/.ssh/authorized_keys" whoami in*)
    let keys_path = "/home/atongen/.ssh/authorized_keys" in

    (* let auth_path  = "/var/log/auth.log" *)
    let auth_path = "/home/atongen/Workspace/personal/whokey/archive/auth.txt" in

    Lwt_main.run (go whoami keys_path auth_path)
