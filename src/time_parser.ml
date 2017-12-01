open Core.Std

type t = Time

let diff t1 t2 =
  Time.abs_diff t1 t2 |> Span.to_sec

let same t1 t2 =
  diff t1 t2 < 1.0

(*
 * ubuntu   pts/5        Mon Oct  2 15:18:21 2017 - Mon Oct  2 15:19:41 2017  (00:01)
 * or
 * ubuntu   pts/0        Sat Oct 28 14:21:11 2017   still logged in
 * or
 * ubuntu   tty7         Mon Oct  2 08:14:16 2017 - crash                     (23:51)
 *)
let of_last str = Time.of_string str
