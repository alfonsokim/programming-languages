open Printf;;
module StringMap = Map.Make(String);;

let put k v mapa = StringMap.add k v mapa;;
let get k mapa = StringMap.find k mapa;;

let get_varname_for var varsMap =
    try
        let old_val = (get var varsMap) + 1 in 
        let new_dict = put var old_val varsMap in 
            (old_val, new_dict)
    with Not_found ->
        let new_dict = put var 1 varsMap in 
            (1, new_dict)


let print_dict key value =
    print_string(key ^ " " ^ string_of_int(value) ^ "\n") ;;


let mapa = put "foo" 0 (put "baz" 1 StringMap.empty) in 
    let (val_one, mapa) = get_varname_for "foo" mapa in  
        print_string ("foo " ^ string_of_int(val_one) ^ "\n");
        let (val_two, mapa) = get_varname_for "foo" mapa in 
        print_string ("foo " ^ string_of_int(val_two) ^ "\n");
