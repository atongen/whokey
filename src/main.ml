open Core.Std

let run cmd =
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_lines inp in
  In_channel.close inp; r

(*
let lasts =
  run "last -FRad"
  |> List.filter (fun x -> Util.contains x "pts/")
	|> List.map (fun x -> Last.of_string x)

let () =
  let auth_strs = run "cat /home/atongen/Workspace/personal/whokey/auth.log | grep 'Accepted publickey for '" in
  let auths = List.map auth_of_str auth_strs in
*)
