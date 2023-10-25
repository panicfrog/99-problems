open Ninety_nine_problems.Ninity_nine;;

(* let print_list = function 
  [] -> print_string "[]";
  | e::l -> print_string "[";
            print_string e;
            List.iter (fun x -> print_string (";"^x)) l;
            print_string "]";; *)

(* 
let () = pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
  |> List.iter print_list ;
  print_newline ();;


let () = encode2 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] |> List.iter (fun item ->
    match item with
    | One e -> print_string ("(" ^  e ^ ", " ^ ")")
    | Many (c, e) -> print_string ("(" ^ string_of_int c ^ ", " ^ e ^ ")"));; *)

(* let () = decode2 [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] |> print_list |> print_newline;; *)

(* let () = drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 |> print_list |> print_newline;; *)
(* let () = slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6 |> print_list |> print_newline;; *)
(* let () = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2) |> print_list |> print_newline;; *)
(* let () =  insert_at "alfa" 4 ["a"; "b"; "c"; "d"] |> print_list |> print_newline;; *)
(* let () = range 4 9 |> List.iter (fun x -> print_int x) |> print_newline;; *)
let () = rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 |> List.iter (fun v -> print_string v);;


