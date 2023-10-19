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