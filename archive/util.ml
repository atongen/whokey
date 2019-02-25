(* https://stackoverflow.com/questions/39813584/how-to-split-on-whitespaces-in-ocaml *)
let split str = Str.split (Str.regexp "[ \n\r\x0c\t]+") str

let sublist l s e =
  let a = Array.of_list l in
  let sub = Array.sub a s e in
  Array.to_list sub

let sublist_str l s e = String.concat " " (sublist l s e)

let contains s1 s2 =
	let re = Str.regexp_string s2 in
	try
		ignore (Str.search_forward re s1 0);
			 true
	with Not_found -> false
