open Core.Std

let run cmd =
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_lines inp in
  In_channel.close inp; r

let lasts =
  let myLasts = run "last -FRad" in
  let pts = List.filter myLasts (fun x -> Util.contains x "pts/") in
	List.map pts Last.of_string

let auths file =
  let lines = In_channel.read_lines file in
  let accepted = List.filter lines (fun x -> Util.contains x "Accepted publickey for ") in
  List.map accepted Auth.of_string

(*
let () =
  let myLasts = lasts in
  let myAuths = auths "/home/atongen/Workspace/personal/whokey/auth.log" in
*)
