open Core.Std
open OUnit2

let ae exp got _test_ctxt = assert_equal exp got
let sae exp got _test_ctxt = assert_equal ~printer:String.to_string exp got

let suite =
  let keys = Keys.from_file "/home/atongen/Workspace/personal/whokey/keys.json" in
  [
    "01">::
      ae (Some "atongen@bellona-2015-10-01") (Keys.find keys "37:c9:85:f8:7d:b7:b8:da:6a:47:3e:ea:97:05:9c:ce")
  ]

let () =
  run_test_tt_main ("whokey tests" >::: suite)
