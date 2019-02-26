open Lwt.Infix

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
    let year = now.tm_year + 1900 in
    make_last_timestamp mon day hour min sec year

let parse_auth_line line keys =
    Scanf.sscanf line "%s %d %d:%d:%d %s sshd[%d]: Accepted publickey for %s from %s port %d ssh2: RSA %s"
    (fun mon day hour min sec _ _ _ host port fingerprint ->
        let comment = match Hashtbl.find_opt keys fingerprint with
        | Some c -> c
        | None -> "[UNKNOWN]" in
        {
            timestamp = make_auth_timestamp mon day hour min sec;
            host;
            port;
            fingerprint = fingerprint_of_string fingerprint;
            comment;
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

let split_lines str = Str.split (Str.regexp "\n+") str


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

let read_process_lines command = split_lines (read_process command)

let build_fingerprint_table keys_path =
    let cmd = Printf.sprintf "cat %s" keys_path in
    let lines = read_process_lines cmd in
    let tbl = Hashtbl.create (List.length lines) in
    List.iter (fun line ->
        (* Open a temporary file for reading and writing. *)
        let name = Filename.temp_file "whokey-" ".tmp" in
        let descr = Unix.openfile name [Unix.O_RDWR] 0o600 in

        (* Write ten lines of output. *)
        let out_channel = Unix.out_channel_of_descr descr in
        Printf.fprintf out_channel "%s\n" line;
        flush out_channel;

        let fingerprint_line = read_process (Printf.sprintf "ssh-keygen -lf %s" name) in
        Scanf.sscanf fingerprint_line "%d %s %s (RSA)"
        (fun _ fingerprint comment -> Hashtbl.add tbl fingerprint comment);

        (* Close the underlying file descriptor and remove the file. *)
        Unix.close descr;
        Sys.remove name;
    ) lines;
    tbl

let find_last_in_auths last auths =
    let maybe_auth = List.find_opt (fun (auth: auth) ->
        abs_float (auth.timestamp -. last.timestamp) <= 1.0
    ) auths in
    match maybe_auth with
    | Some auth -> Some (last, auth)
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
    | StillLoggedIn -> "still logged in         "
    | LoggedOut f -> format_time f

let last_auth_pair_to_string last auth =
    Printf.sprintf "%s - %s %s %s %s"
        (format_time last.timestamp)
        (status_to_string last.status)
        auth.comment
        last.pts
        auth.host

let compare_last_tuple (l0, _) (l1, _) = compare l0.timestamp l1.timestamp

let go whoami keys_path auth_path_0 auth_path_1 =
    (* keys *)
    let keys = build_fingerprint_table keys_path in

    (* auth *)
    let auth_match = Printf.sprintf "Accepted publickey for %s" whoami in
    let auth_stream = Lwt_process.pread_lines ("", [|"cat"; auth_path_0; auth_path_1|]) in
    let auths = Lwt_stream.filter_map (fun line ->
        if contains line auth_match then
            Some (parse_auth_line line keys)
        else None
    ) auth_stream
    |> Lwt_stream.to_list in

    (* last *)
    auths >>= fun auth_list ->
    let last_stream = Lwt_process.pread_lines ("", [|"last"; "-Fa"; whoami|]) in
    let lasts_auths = Lwt_stream.filter_map (fun line ->
        let maybe_last = parse_last_line line whoami in
        match maybe_last with
        | Some last -> find_last_in_auths last auth_list
        | None -> None
    ) last_stream in

    Lwt_stream.to_list lasts_auths >>= fun results ->
    let sorted = List.sort compare_last_tuple results in
    List.iter (fun (last, auth) ->
       print_endline (last_auth_pair_to_string last auth)
    ) sorted;

    Lwt.return_unit

let () =
    let whoami = if Array.length Sys.argv > 1
        then Sys.argv.(1)
        else read_process "whoami" |> String.trim
    in
    let keys_path = Printf.sprintf "/home/%s/.ssh/authorized_keys" whoami in
    let auth_path_0  = "/var/log/auth.log" in
    let auth_path_1  = "/var/log/auth.log.1" in
    Lwt_main.run (go whoami keys_path auth_path_0 auth_path_1)
