open Core.Std
open OUnit2

let ae exp got _test_ctxt = assert_equal exp got
let sae exp got _test_ctxt = assert_equal ~printer:String.to_string exp got
let fae exp got _test_ctxt = assert_equal ~printer:Float.to_string exp got

let suite =
  let keys = Keys.from_file "/home/atongen/Workspace/personal/whokey/keys.json" in
  [
    "keys-00">::
      ae (Some "atongen@bellona-2015-10-01") (Keys.find keys "37:c9:85:f8:7d:b7:b8:da:6a:47:3e:ea:97:05:9c:ce");

    "time_parser-00">::
      fae 1512166872.0 (Time_parser.of_last "Dec  1 16:21:12 2017" |> Time_parser.epoch);

    "time_parser-01">::
      fae 1509144783.0 (Time_parser.of_auth "Oct 27 17:53:03" |> Time_parser.epoch);
  ]

let () =
  run_test_tt_main ("whokey tests" >::: suite)
