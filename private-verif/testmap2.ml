module StringMap = Map.Make (String)

let histogram lst =
  List.fold_left (fun m key ->
    let n =
      if StringMap.mem key m
      then succ (StringMap.find key m)
      else 1
    in
    Printf.printf "key[%s] val[%i]\n" key n ;
    StringMap.add key n m
  ) StringMap.empty lst

let () =
  let h = histogram ["a"; "b"; "a"; "c"; "b"; "b"] in
  StringMap.iter (fun key value ->
    Printf.printf " %s: %d\n" key value
  ) h