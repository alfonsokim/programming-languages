
(** Variable definition handler
  *)

module StringMap = Map.Make(String);;


(** get the next variable number for a given prefix,
    exaple: get_varname_for "e" -> "e1"
            get_varname_for "e" -> "e2"
            get_varname_for "a" -> "a1"
  *)
let get_varname_for var varsMap =
	try
		let old_val = (StringMap.find var varsMap) + 1 in 
		let new_dict = StringMap.add var old_val varsMap in 
			(var ^ string_of_int(old_val), new_dict)
	with Not_found ->
    	let new_dict = StringMap.add var 1 varsMap in 
    		(var ^ "1", new_dict)


(** For debugging the dictionary, not used
  *)
let print_dict key value =
	print_string(key ^ " " ^ string_of_int(value) ^ "\n") ;;

