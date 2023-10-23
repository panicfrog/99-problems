open Ninety_nine_problems.Ninity_nine;;

let print_list = function 
  [] -> print_string "[]";
  | e::l -> print_string "[";
            print_string e;
            List.iter (fun x -> print_string (";"^x)) l;
            print_string "]";;


let () = pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
  |> List.iter print_list ;
  print_newline ();;


let () = encode2 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] |> List.iter (fun item ->
    match item with
    | One e -> print_string ("(" ^  e ^ ", " ^ ")")
    | Many (c, e) -> print_string ("(" ^ string_of_int c ^ ", " ^ e ^ ")"));;
  