open Core

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

let lasts last_list =
  List.filter last_list ~f:(fun x -> Util.contains x "pts/") |>
	List.map ~f:Last.of_string

let get_last_list =
  (* Util.run "last -FRad" *)
  let cwd = Unix.getcwd() in
  lasts (Util.get_file_lines (cwd ^ "/last.log"))

let auth_log_base =
  (* "/var/log/auth.log" *)
  let cwd = Unix.getcwd() in
  cwd ^ "/auth.log"

let get_auth_list =
  let pathname idx =
    let base = auth_log_base in
    match idx with
    | 0 -> base
    | n -> base ^ "." ^ (Int.to_string n)
  in
  let rec aux idx ac =
    let path = pathname idx in
      if Util.file_exists path; then
        let lines = Util.get_file_lines path |>
          List.filter ~f:(fun x -> Util.contains x "Accepted publickey for ") in
        aux (idx + 1) (List.concat [ac; lines])
      else
        ac
  in
  aux 0 [] |>
  List.map ~f:Auth.of_string |>
  List.sort ~cmp:Auth.compare

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