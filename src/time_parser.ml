type t = float

let diff t1 t2 =
  abs_float (t2 -. t1)

let same t1 t2 =
  diff t1 t2 <= 1.0

let months =
  ["Jan", 0; "Feb", 1; "Mar", 2; "Apr", 3; "May",  4; "Jun",  5;
   "Jul", 6; "Aug", 7; "Sep", 8; "Oct", 9; "Nov", 10; "Dec", 11]

let of_last str =
  Scanf.sscanf str "%s %d %d:%d:%d %d"
    (fun mon day hour min sec year ->
      let open Unix in
      let mon = List.assoc mon months in
      fst (mktime {
        tm_sec=sec;
        tm_min=min;
        tm_hour=hour;
        tm_mon=mon;
        tm_mday=day;
        tm_year=year-1900;
        tm_wday=0;
        tm_yday=0;
        tm_isdst=false
      })
    )

let of_auth str =
  Scanf.sscanf str "%s %d %d:%d:%d"
    (fun mon day hour min sec ->
      let open Unix in
      let now = localtime (Unix.time ()) in
      let mon = List.assoc mon months in
      fst (mktime {
        tm_sec=sec;
        tm_min=min;
        tm_hour=hour;
        tm_mon=mon;
        tm_mday=day;
        tm_year=now.tm_year;
        tm_wday=0;
        tm_yday=0;
        tm_isdst=false
      })
    )

let epoch x = x

let of_float x = x

let pretty x =
  let open Unix in
  let u = localtime x in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
    (u.tm_year + 1900)
    (u.tm_mon + 1)
    u.tm_mday
    u.tm_hour
    u.tm_min
    u.tm_sec