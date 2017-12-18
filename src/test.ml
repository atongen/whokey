open Core.Std
open OUnit2

let ae exp got _test_ctxt = assert_equal exp got
let sae exp got _test_ctxt = assert_equal ~printer:String.to_string exp got
let fae exp got _test_ctxt = assert_equal ~printer:Float.to_string exp got
let aae exp got _test_ctxt = assert_equal ~printer:Auth.to_string exp got
let lae exp got _test_ctxt = assert_equal ~printer:Last.to_string exp got

let suite =
  let keys = Keys.from_file "/home/atongen/Workspace/personal/whokey/keys.json" in
  [
    "keys-00">::
      ae (Some "atongen@bellona-2015-10-01") (Keys.find keys "37:c9:85:f8:7d:b7:b8:da:6a:47:3e:ea:97:05:9c:ce");

    "time_parser-00">::
      fae 1512166872.0 (Time_parser.of_last "Dec  1 16:21:12 2017" |> Time_parser.epoch);

    "time_parser-01">::
      fae 1509144783.0 (Time_parser.of_auth "Oct 27 17:53:03" |> Time_parser.epoch);

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
  ]

let () =
  run_test_tt_main ("whokey tests" >::: suite)
