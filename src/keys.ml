open Core.Std

(* for x in certificates/*; do ssh-keygen -E md5 -l -f $x 2>/dev/null; done *)

type t = string String.Map.t

let alist_from_file path =
  let json = Yojson.Basic.from_file path in
  let alist_json = Yojson.Basic.Util.to_assoc json in
  List.map alist_json (fun (fingerprint, key_json) ->
    (fingerprint, key_json |> Yojson.Basic.Util.to_string))

let from_file path =
  let alist = alist_from_file path in
  Map.of_alist_exn alist ~comparator:String.comparator;;

let find keys key = Map.find keys key
