open Ninety_nine_problems.Ninity_nine;;

let print_list = function 
  [] -> print_string "[]";
  | e::l -> print_string "[";
            print_string e;
            List.iter (fun x -> print_string (";"^x)) l;
            print_string "]";;

(* 
let () = pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
  |> List.iter print_list ;
  print_newline ();;


let () = encode2 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] |> List.iter (fun item ->
    match item with
    | One e -> print_string ("(" ^  e ^ ", " ^ ")")
    | Many (c, e) -> print_string ("(" ^ string_of_int c ^ ", " ^ e ^ ")"));; *)

(* let () = decode2 [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] |> print_list |> print_newline;; *)

let () = drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 |> print_list |> print_newline;;

let l1 = [1;2;]
let l2 = 3::l1
let () = List.iter (fun x -> print_int x) l2;;
