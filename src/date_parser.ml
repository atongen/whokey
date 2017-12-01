(* Association list mapping month abbreviations to 0-based month
   numbers as required by Unix.mktime. *)
let months =
  ["Jan", 0; "Feb", 1; "Mar", 2; "Apr", 3; "May", 4; "Jun", 5;
   "Jul", 6; "Aug", 7; "Sep", 8; "Oct", 9; "Nov", 10; "Dec", 11]

(* Turn a time zone into an offset in minutes. Not exhaustive. *)
let parse_tz = function
  | "" | "Z" | "GMT" | "UTC" | "UT" -> 0
  | "PST" -> -480
  | "MST" | "PDT" -> -420
  | "CST" | "MDT" -> -360
  | "EST" | "CDT" -> -300
  | "EDT" -> -240
  | string ->
      Scanf.sscanf string "%c%02d%_[:]%02d"
        (fun sign hour min ->
           min + hour * (if sign = '-' then -60 else 60))

(* List of date-parsing functions from strings to epoch seconds. *)
let date_parsers =
  [
    (fun string ->
       Scanf.sscanf string "%d %s %d %d:%d:%d %s"
         (fun mday mon year hour min sec tz ->
            let mon = List.assoc mon months in
            fst (Unix.mktime
                   {Unix.tm_sec=sec; tm_min=min; tm_hour=hour;
                    tm_mday=mday; tm_mon=mon; tm_year=year-1900;
                    tm_wday=0; tm_yday=0; tm_isdst=false})
            -. (float (parse_tz tz) *. 60.0)));
    (fun string ->
       Scanf.sscanf string "%3s, %d %s %4d %d:%d:%d %s"
         (fun wday mday mon year hour min sec tz ->
            let mon = List.assoc mon months in
            fst (Unix.mktime
                   {Unix.tm_sec=sec; tm_min=min; tm_hour=hour;
                    tm_mday=mday; tm_mon=mon; tm_year=year-1900;
                    tm_wday=0; tm_yday=0; tm_isdst=false})
            -. (float (parse_tz tz) *. 60.0)));
    (fun string ->
       Scanf.sscanf string "%3s, %d %s %2d %d:%d:%d %s"
         (fun wday mday mon year hour min sec tz ->
            let mon = List.assoc mon months in
            fst (Unix.mktime
                   {Unix.tm_sec=sec; tm_min=min; tm_hour=hour;
                    tm_mday=mday; tm_mon=mon; tm_year=year;
                    tm_wday=0; tm_yday=0; tm_isdst=false})
            -. (float (parse_tz tz) *. 60.0)));
  ]

(* Tries each of the above date parsers, one at a time, until one
   of them doesn't throw an exception. If they all fail, returns
   a value of 0.0. *)
let getdate string =
  let result = ref 0.0 in
  let parsers = ref date_parsers in
  while !result = 0.0 && !parsers <> [] do
    let parse = List.hd !parsers in
    parsers := List.tl !parsers;
    try result := parse string with _ -> ()
  done;
  !result

(* Formats a date given in epoch seconds for display. *)
let fmtdate epoch =
  let tm = Unix.localtime epoch in
  Printf.sprintf "%02d:%02d:%02d %04d/%02d/%02d"
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
