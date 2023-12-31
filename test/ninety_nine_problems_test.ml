open Ninety_nine_problems.Ninity_nine;;

let%test "[ 1. last ]" = match last [ "a" ; "b" ; "c" ; "d" ] with
  | Some x -> x = "d"
  | None -> false
;;

let%test "[2. last_two 1]" = match last_two [ "a" ; "b" ; "c" ; "d" ] with
  | Some (x, y) -> x = "c" && y = "d"
  | None -> false
;;
let%test "[2. last_two 2]" = match last_two ["a"] with
  | Some (_, _) -> false
  | None -> true
;;

let%test "[3. at 1]" = at 1 [ "a" ; "b" ; "c" ; "d" ] = Some "b";;
let%test "[3. at 2]" = at 3 ["a"] = None;;

let%test "[4. length]" = length [ "a" ; "b" ; "c" ; "d" ] = 4;;

let%test "[5. rev]" = rev [ "a" ; "b" ; "c" ] = [ "c" ; "b" ; "a" ];;

let%test "[6. is_palindrome 1]" = is_palindrome ["x"; "a"; "m"; "a"; "x"];;
let%test "[6. is_palindrome 2]" = not @@ is_palindrome ["a"; "b"];;

let%test "[7. flatten]" = flatten [One "a"; Many [One "b"; Many [One "c"; One "d"]; One "e"]] = ["a"; "b"; "c"; "d"; "e"];;

let%test "[8. compress 1]" = compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = ["a"; "b"; "c"; "a"; "d"; "e"];;
let%test "[8. compress 2]" = compress ["a";"a";"a";"a";] = ["a"];;

let%test "[9. pack]" = pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]];;

let%test "[10. encode]" = encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")];;

let%test "[11. encode2]" = encode2 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
Many (4, "e")];;

let%test "[12. decode]" = decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

let%test "[13. decode2]" = decode2 [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

let%test "[14. duplicate]" = duplicate ["a";"b";"c";"c";"d"] = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"];;

let%test "[15. replicate]" = replicate ["a"; "b"; "c"] 3 = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"];;

let%test "[16. drop]" = drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 = ["a"; "b"; "d"; "e"; "g"; "h"; "j"];;

let%test "[17. split]" = split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]);;
let%test "[17. split 2]" = split ["a"; "b"; "c"; "d"] 5 = (["a"; "b"; "c"; "d"], []);;

let%test "[18. slice]" = slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6 = ["c"; "d"; "e"; "f"; "g"];;

let%test "[19. rotate]" = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"];;
let%test "[19. rotate]" = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2) = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"];;

let%test  "[20. remove_at]" = remove_at 1 ["a"; "b"; "c"; "d"] = ["a"; "c"; "d"];;

let%test "[21. insert_at]" = insert_at "alfa" 1 ["a"; "b"; "c"; "d"] = ["a"; "alfa"; "b"; "c"; "d"];;
let%test "[21. insert_at 2]" = insert_at "alfa" 3 ["a"; "b"; "c"; "d"] = ["a"; "b"; "c"; "alfa"; "d"];;
let%test "[21. insert_at 3]" = insert_at "alfa" 4 ["a"; "b"; "c"; "d"] = ["a"; "b"; "c"; "d"; "alfa"];;

let%test "[22. range]" = range 4 9 = [4; 5; 6; 7; 8; 9];;
let%test "[22. range]" = range 9 4 = [9; 8; 7; 6; 5; 4];;

let%test "[26. extract]" = extract 2 ["a"; "b"; "c"; "d"] = [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]];;