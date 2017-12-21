open Core.Std
open OUnit2

let ae exp got _test_ctxt = assert_equal exp got

let sae exp got _test_ctxt = assert_equal ~printer:String.to_string exp got
let iae exp got _test_ctxt = assert_equal ~printer:Int.to_string exp got
let bae exp got _test_ctxt = assert_equal ~printer:Bool.to_string exp got

let fae exp got _test_ctxt = assert_equal ~printer:Float.to_string exp got
let aae exp got _test_ctxt = assert_equal ~printer:Auth.to_string exp got
let lae exp got _test_ctxt = assert_equal ~printer:Last.to_string exp got

let keys = Keys.from_file "/home/atongen/Workspace/personal/whokey/keys.json"

let t1 = Time_parser.of_float 1513900591.0
let t2 = Time_parser.of_float 1513902285.0
let t3 = Time_parser.of_float 1513902286.0

let auth1 = Auth.of_string "Dec 21 17:56:31 drip-staging-ansible sshd[29664]: Accepted publickey for ubuntu from 24.52.56.2 port 57126 ssh2: RSA SHA256:A7QkW++563PAB+LEs6JD/mB+YyC7OUI2p5UsPb+25vA"
let auth2 = Auth.of_string "Dec 21 18:24:45 drip-staging-ansible sshd[29829]: Accepted publickey for ubuntu from 216.70.43.154 port 20966 ssh2: RSA SHA256:auz5n5RrA1X0IFXvaZr7VEuR/WpsA9FIOPS1yGtaRzI"
let auths = [auth1; auth2]

let last1 = Last.of_string "ubuntu   pts/0        Thu Dec 21 18:24:45 2017   still logged in"
let last2 = Last.of_string "ubuntu   pts/0        Thu Dec 21 17:56:31 2017 - Thu Dec 21 18:16:32 2017  (00:20)"

let suite =
  [
    "keys-00">::
      ae (Some "atongen@bellona-2015-10-01") (Keys.find keys "37:c9:85:f8:7d:b7:b8:da:6a:47:3e:ea:97:05:9c:ce");

    "time_parser-00">::
      fae 1512166872.0 (Time_parser.of_last "Dec  1 16:21:12 2017" |> Time_parser.epoch);

    "time_parser-01">::
      fae 1509144783.0 (Time_parser.of_auth "Oct 27 17:53:03" |> Time_parser.epoch);

    "Time_parser-02">::
      bae false (Time_parser.same t1 t2);

    "Time_parser-03">::
      bae true (Time_parser.same t2 t3);

    "auth-00">::
      sae "1509144783 john 37:c9:85:f8:7d:b7:b8:dd:6a:42:3e:ca:97:05:9c:ce"
        (Auth.of_string "Oct 27 17:53:03 some-host sshd[14313]: Accepted publickey for john from 211.11.11.111 port 32876 ssh2: RSA 37:c9:85:f8:7d:b7:b8:dd:6a:42:3e:ca:97:05:9c:ce" |> Auth.to_string);

    "last-00">::
      sae "1506975501 ubuntu pts/5"
        (Last.of_string "ubuntu   pts/5        Mon Oct  2 15:18:21 2017 - Mon Oct  2 15:19:41 2017  (00:01)" |> Last.to_string);
    "last-01">::
      sae "1509218471 ubuntu pts/0"
        (Last.of_string "ubuntu   pts/0        Sat Oct 28 14:21:11 2017   still logged in" |> Last.to_string);
    "last-02">::
      sae "1506950056 ubuntu tty7"
        (Last.of_string "ubuntu   tty7         Mon Oct  2 08:14:16 2017 - crash                     (23:51)" |> Last.to_string);

    "last-03">::
      bae true (Last.is_auth last1 auth2);
    "last-04">::
      bae true (Last.is_auth last2 auth1);

    "last-05">::
      bae false (Last.is_auth last1 auth1);
    "last-06">::
      bae false (Last.is_auth last2 auth2);

    "last-07">::
      iae 1 (List.length (Last.find_auths last2 auths));
  ]

let () =
  run_test_tt_main ("whokey tests" >::: suite)
