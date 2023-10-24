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