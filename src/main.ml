open Core.Std

(* for x in certificates/*; do ssh-keygen -l -f $x 2>/dev/null; done *)

let keys_alist = [
  "SHA256:v+4hK/EOo7Wl2akOEJvYBXRKQnJ4TE6YnmgsPOiFqrw", "aleckloss@Alec-K-MBP.local";
  "SHA256:auz5n5RrA1X0IFXvaZr7VEuR/WpsA9FIOPS1yGtaRzI", "atongen@bellona-2015-10-01";
  "SHA256:jkSLJHqf3MjmzvzvR75yGPUz9DYOq6LkjQ4IkV3y+Fw", "brian.reed@ave81.com";
  "SHA256:ufQhqomTWGXPsjEdHWxyMRCIKXU+WfPwMTjmVZmAIkM", "charlie.bevis@drip.com";
  "SHA256:zDX1qnfEemeM/RoCkHJlYiYgq2rU8OnHrBrw3pI57aQ", "cjamison@cjamison-desktop";
  "SHA256:bzX+hm/fJLRRiaLJqynyVLSnIazRtkmu8iW2Wo89GtA", "derrickreimer@gmail.com";
  "SHA256:13wlMClMazrthE2Hu3E4elAJnDZssF98XrG2AefnCpA", "aleckoss@Alec-K-MBP.local";
  "SHA256:+ImShMUk/kLcIppmI7SsQ2dxMvsadHWGS9UEhu/V2gU", "drip-ansible-key";
  "SHA256:UdmWQJsmcNtB+CQLIlLTyBFOLml6Oo84mS0T5o//3C0", "drip-staging-ansible-key";
  "SHA256:A7QkW++563PAB+LEs6JD/mB+YyC7OUI2p5UsPb+25vA", "dustin.blomquist@gmail.com";
  "SHA256:379V5CyHueNF2wpb9Cp+Jb+xPsC3+yYgJGRW8zGFFNQ", "iantnance@gmail.com";
  "SHA256:wBhnyZyHS1YIgd6xhEclREOGnwpaui+GTAY4jQIAZ84", "jasonselby@Jason-Selbys-MBP.local";
  "SHA256:CZkv+PGEEAOGLEHUITgIzncSp/OQ2/7lELzqnekKa/s", "jonhanson@Jon-H-MBP.local";
  "SHA256:1tPElw97npyM62gB6p1UIyCTJH4sO0LQPr0EaiKQy9c", "michaelnadel@Michael-N-MBP.local";
  "SHA256:jos06T3YMAXtfDZXo47abIDhyeUTCoFLnwlkP6b8u9E", "robynsamuda@Robyn-S-MBP.local";
  "SHA256:7e593TUot6zAhaV76x3kYXK3EtEyabB4S2Nv1sMkWgg", "steven@stevenbone.com";
  "SHA256:IAGa/vfcilXiI3S+qDeUAfoND069uysjPmkOtlVRzuE", "taylor.sampson@ave81.com";
  "SHA256:AwTDjCC/h6FHF8bfK+fUGY7Txx3jQyzAwW8vvbdda4g", "tim@Dragon.local";
  "SHA256:QzE0xkArNwiawIurNYlpSQYv00JHMgIhQGn7RJAkdDQ", "timothybreitkreutz@Tim-B-iMac";
  "SHA256:8j/lY54vi4+VQPXZEoAKfTgtNeU1tUBFxMPHt7eVVLs", "trevorhaag@Trevor-H-MBP.local";
]

let keys = Keys.from_alist keys_alist

let run cmd =
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_lines inp in
  In_channel.close inp; r

let get_file_lines file = In_channel.read_lines file

let lasts last_list =
  let pts = List.filter last_list ~f:(fun x -> Util.contains x "pts/") in
	List.map pts ~f:Last.of_string

let auths auth_list =
  let accepted = List.filter auth_list ~f:(fun x -> Util.contains x "Accepted publickey for ") in
  List.map accepted ~f:Auth.of_string

let get_last_list =
  (* run "last -FRad" *)
  let cwd = Unix.getcwd() in
  lasts (get_file_lines (cwd ^ "/last.log"))

let get_auth_list =
  (* auths (get_file_lines "/var/log/auth.log") *)
  let cwd = Unix.getcwd() in
  auths (get_file_lines (cwd ^ "/auth.log"))

let auth_id { Auth.fingerprint = f; _ } =
  match Keys.find keys f with
  | None    -> "[UNKNOWN]"
  | Some(x) -> x

let print_auths { Last. user; timestamp; pts } auths =
  List.iter auths ~f:(fun auth ->
    let id = auth_id auth in
      Printf.printf "%s %s %s %s\n" (Time_parser.pretty timestamp) id user pts
  )

let () =
  let myLasts = get_last_list in
  let myAuths = get_auth_list in
  List.iter myLasts ~f:(fun last ->
    let auths = Last.find_auths last myAuths in
      print_auths last auths
  )