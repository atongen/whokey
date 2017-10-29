(* for x in certificates/*; do ssh-keygen -E md5 -l -f $x 2>/dev/null; done *)
let keys = [
  "37:c9:85:f8:7d:b7:b8:da:6a:47:3e:ea:97:05:9c:ce", "atongen@bellona-2015-10-01";
  "7f:f5:ce:84:b0:b8:df:c4:31:b7:87:6b:98:ff:12:99", "brian.reed@ave81.com";
  "60:74:ea:fd:39:7a:c9:52:35:79:5b:31:f2:5c:d9:52", "cjamison@cjamison-desktop";
  "c2:db:5c:cc:df:64:7d:8e:b3:23:0c:e9:d4:84:71:4e", "derrickreimer@gmail.com";
  "14:ff:25:35:d6:04:43:2f:31:eb:57:21:6d:cf:b1:6a", "drip-ansible-key";
  "6e:33:de:b0:9f:6f:f1:44:e6:c6:16:b3:9c:78:66:b3", "drip-staging-ansible-key";
  "0a:92:94:05:db:92:de:92:00:ac:e6:d4:47:c4:ec:0a", "dustin.blomquist@gmail.com";
  "ab:30:87:55:f8:aa:17:8b:e2:0a:6b:66:ad:33:ae:3d", "iantnance@gmail.com";
  "f5:1f:7a:d7:78:3d:f5:36:c8:f2:56:9d:53:f7:a2:c9", "jonhanson@Jon-H-MBP.local";
  "39:17:56:b2:01:f1:5f:f0:56:fe:dc:eb:19:00:3d:3c", "michaelnadel@Michael-N-MBP.local";
  "5a:62:f0:a9:89:09:2c:9f:bf:69:33:a1:df:57:0e:df", "robynsamuda@Robyn-S-MBP.local";
  "85:6e:27:49:3f:fa:a0:0f:ee:11:4d:e9:5d:32:de:68", "steven@stevenbone.com";
  "de:2e:37:ca:56:c9:62:10:18:1e:25:ff:fa:3d:6f:ac", "taylor.sampson@ave81.com";
  "45:53:65:5c:2c:fb:21:f3:bf:fc:e7:93:15:60:a4:5a", "tim@Dragon.local";
  "94:7e:00:6d:8b:22:77:c5:8d:fe:9a:f9:6f:95:65:40", "timothybreitkreutz@Tim-B-iMac";
]

(* https://stackoverflow.com/questions/39813584/how-to-split-on-whitespaces-in-ocaml *)
let split str = Str.split (Str.regexp "[ \n\r\x0c\t]+") str

let sublist l s e =
  let a = Array.of_list l in
  let sub = Array.sub a s e in
  Array.to_list sub

let sublist_str l s e = String.concat " " (sublist l s e)

let rec each f l =
  match l with
  | [] -> ()
  | hd::tl -> f hd; each f tl

let print_list l = each (fun x -> print_string (x ^ "\n")) l

let run cmd =
  let inp = Unix.open_process_in cmd in
  let r = Core.Std.In_channel.input_lines inp in
  Core.Std.In_channel.close inp; r

(*
 * ubuntu   pts/5        Mon Oct  2 15:18:21 2017 - Mon Oct  2 15:19:41 2017  (00:01)
 * or
 * ubuntu   pts/0        Sat Oct 28 14:21:11 2017   still logged in
 * or
 * ubuntu   tty7         Mon Oct  2 08:14:16 2017 - crash                     (23:51)
 *)
type last = { user: string; pts: string; timestamp: string }

let last_of_str str =
  let tokens = split str in
  {
    user = List.nth tokens 0;
    pts = List.nth tokens 1;
    timestamp = sublist_str tokens 3 3;
  }

let str_of_last { user = u; pts = p; timestamp = t } =
  Printf.sprintf "u: '%s' pts: '%s' ts: '%s'" u p t

(*
 * Oct 27 17:53:03 drip-production-ansible sshd[14313]: Accepted publickey for ubuntu from 216.70.43.154 port 32876 ssh2: RSA 37:c9:85:f8:7d:b7:b8:da:6a:47:3e:ea:97:05:9c:ce
 *)
type auth = { user: string; timestamp: string; ip: string; fingerprint: string }

let auth_of_str str =
  let tokens = split str in
  {
    user = List.nth tokens 8;
    timestamp = sublist_str tokens 0 3;
    ip = List.nth tokens 10;
    fingerprint = List.nth tokens 15;
  }

let str_of_auth { user = u; timestamp = t; ip = i; fingerprint = f; } =
  Printf.sprintf "u: '%s' ts: '%s' ip: '%s' fp = '%s'" u t i f

let () =
  let last_strs = run "last -FRad -n 100 | grep 'pts/'" in
  let lasts = List.map last_of_str last_strs in
  let auth_strs = run "cat /home/atongen/Workspace/personal/whokey/auth.log | grep 'Accepted publickey for '" in
  let auths = List.map auth_of_str auth_strs in
