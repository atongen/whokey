open Core.Std

type t = string String.Map.t

let alist_from_file path =
  Yojson.Basic.from_file path |>
  Yojson.Basic.Util.to_assoc |>
  List.map ~f:(fun (fingerprint, key_json) ->
    (fingerprint, key_json |> Yojson.Basic.Util.to_string))

let from_alist alist =
  Map.of_alist_exn alist ~comparator:String.comparator

let from_file path =
  from_alist (alist_from_file path)

let find keys key = Map.find keys key
