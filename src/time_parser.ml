type t = float

let diff t1 t2 =
  t2 -. t1

let same t1 t2 =
  diff t1 t2 <= 1.0

let months =
  ["Jan", 0; "Feb", 1; "Mar", 2; "Apr", 3; "May",  4; "Jun",  5;
   "Jul", 6; "Aug", 7; "Sep", 8; "Oct", 9; "Nov", 10; "Dec", 11]

(*
 * ubuntu   pts/5        Mon Oct  2 15:18:21 2017 - Mon Oct  2 15:19:41 2017  (00:01)
 * or
 * ubuntu   pts/0        Sat Oct 28 14:21:11 2017   still logged in
 * or
 * ubuntu   tty7         Mon Oct  2 08:14:16 2017 - crash                     (23:51)
 *)
let of_last str =
  Scanf.sscanf str "%s %d %d:%d:%d %d"
    (fun mon day hour min sec year ->
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
    )

(*
 * Oct 27 17:53:03 drip-production-ansible sshd[14313]: Accepted publickey for ubuntu from 216.70.43.154 port 32876 ssh2: RSA 37:c9:85:f8:7d:b7:b8:da:6a:47:3e:ea:97:05:9c:ce
 *)
let of_auth str =
  Scanf.sscanf str "%s %d %d:%d:%d"
    (fun mon day hour min sec ->
      let now = Unix.localtime (Unix.time ()) in
      let mon = List.assoc mon months in
      fst (Unix.mktime {
        Unix.tm_sec=sec;
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
