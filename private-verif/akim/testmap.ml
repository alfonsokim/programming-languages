open Printf ;;

module StringMap = Map.Make(String)

let rec count m ke =
    match ke with
    | [] -> m
    | hd :: tl -> count (StringMap.add hd 1 m) tl


let put var varsMap = 
    let f key value = if key == var then value + 1 else value in 
    StringMap.map f varsMap ;;

let get_varname_for var varsMap = 
    try
        let value = StringMap.find var varsMap in
        let varsMap = StringMap.add var (value+1) varsMap in 
        Printf.printf "Val: %i\n" (StringMap.find var varsMap) 
    with Not_found ->
        (* StringMap.add var 1 varsMap ;; *)
        Printf.printf "Var nueva: %s\n" var ;;

let print_dict key value =
    print_string(key ^ " " ^ string_of_int(value) ^ "\n") ;;

let main () =
    (* get_varname_for "" StringMap.empty *)
    let m = count StringMap.empty ["foo"; "bar"; "baz"] in 
    let a = get_varname_for "foo" m in
    get_varname_for "foo" m 
    (* let newval = ( put ("foo") m ) in *)
    (* put ("foo") m ; *)
    (* StringMap.iter print_dict m ;; *)

let () = main ()
