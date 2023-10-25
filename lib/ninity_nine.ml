
(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)
let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t
;;

(* 2. Find the last but one (last and penultimate) elements of a list. (easy) *)
let rec last_two = function
  | [] -> None
  | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: y :: t -> last_two @@ y :: t
;;

(* 3. Find the K'th element of a list. (easy) *)
let rec at i = function 
  | [] -> None
  | _ when i < 0 -> None
  | h :: _ when i = 0 -> Some h
  | _ :: t -> at (i - 1) t
;;

(* 4. Find the number of elements of a list. (easy) *)
let length l =  
  let rec aux acc = function
    | [] -> acc
    | _ :: t -> aux (acc + 1) t in
  aux 0 l
;;


(* 5. Reverse a list. (easy) *)
let rec rev = function 
  | [] -> []
  | h :: t -> rev t @ [h]
;;

(* 6. Find out whether a list is a palindrome. (easy) *)
let is_palindrome = function
  | [] -> true
  | l -> l = rev l
;;

(* 7. Flatten a nested list structure. (medium) *)
type 'a node =
  | One of 'a 
  | Many of 'a node list;;
let rec flatten = function 
  | [] -> []
  | One h :: t -> h :: flatten t
  | Many h :: t -> flatten h @ flatten t
;;

(* 8. Eliminate consecutive duplicates of list elements. (medium) *)
let compress = function 
  | [] -> []
  | h :: t ->  
  let rec aux pre = function 
    | [] -> []
    | h :: t -> if h = pre then aux pre t else h :: aux h t in
  h :: aux h t
;;

(* 9. Pack consecutive duplicates of list elements into sublists. (medium) *)
let pack = function 
  | [] -> []
  | h :: t -> 
  let rec aux cur cl acc = function 
    [] -> cl :: acc
    | h :: t -> if h = cur then aux cur (h :: cl) acc t else aux h [h] (cl :: acc) t in
  List.rev @@ aux h [h] [] t
;;

(* 10. Run-length encoding of a list. (easy) *)
let encode = function 
  | [] -> []
  | h :: t ->
    let rec aux cur cc acc = function
    | [] -> (cc, cur) :: acc
    | h :: t -> if h = cur then aux cur (cc + 1) acc t else aux h 1 ((cc, cur) :: acc) t in
    List.rev @@ aux h 1 [] t
;;


(* 11. Modified run-length encoding. (easy) *)
type 'a rle =
    | One of 'a
    | Many of int * 'a
;;
let encode2 = function 
  | [] -> []
  | h :: t -> 
    let f e c = if c = 1 then One e else Many (c, e) in
    let rec aux cur count acc = function 
    | [] -> acc @ [f cur count]
    | h :: t -> if h = cur then aux cur (count + 1) acc t else aux h 1 (acc @ [f cur count]) t in
  aux h 1 [] t
;;

(* 12. Decode a run-length encoded list. (medium) *)
let decode l =  
    let transfer  =  function 
      | One e -> [e]
      | Many (c, e) -> 
        let rec taux e c acc = if c = 0 then acc else taux e (c - 1) (e :: acc ) in
        taux e c [] in
    List.map transfer l |> List.flatten
;;
  
(* 13. Run-length encoding of a list (direct solution). (medium) *)
let decode2 = function
| [] -> []
| h :: t ->
    let rec aux cur count acc = function 
    | [] -> if count = 0 then acc else aux cur (count - 1) (cur :: acc) []
    | h :: t -> 
      if count = 0 then
        match h with 
        | One e -> aux e 1 acc t
        | Many (c, e) -> aux e c acc t
      else 
        aux cur (count - 1) (cur :: acc) (h :: t) in
    match h with 
    | One e -> List.rev @@ aux e 1 [] t
    | Many (c, e) -> List.rev @@ aux e c [] t
;;

(* 14. Duplicate the elements of a list. (easy) *)
let duplicate = function
  | [] -> []
  | h :: t -> 
    let rec aux acc = function 
      | [] -> acc
      | h :: t -> aux (h :: h :: acc) t in
    List.rev @@ aux [h;h;] t
;;

(* 15. Replicate the elements of a list a given number of times. (medium) *)
let replicate l times = 
  let rep c e = 
    let rec aux e c acc = if c = 0 then acc else aux e (c - 1) (e :: acc ) in
    aux e c [] in
  List.map (fun i -> rep times i) l |> List.flatten
;;

(* 16. Drop every N'th element from a list. (medium) *)
let drop l n = 
  let rec aux i acc = function 
    | [] -> acc
    | h :: t -> if i mod n = 0 then aux (i + 1) acc t else aux (i + 1) (h :: acc) t in
  List.rev @@ aux 1 [] l 
;;

(* 17. Split a list into two parts; the length of the first part is given. (easy) *)
let split l n = 
  let rec aux i acc = function 
  | [] -> (List.rev acc , [])
  | h :: t -> if i = n then (List.rev acc , h :: t) else aux (i + 1) (h :: acc) t in
  aux 0 [] l
;;

(* 18. Extract a slice from a list. (medium) *)
let slice l i k = 
  let rec aux idx acc = function 
  | [] -> acc
  | h :: t -> if idx >= i && idx <= k then aux (idx + 1) (h :: acc) t else aux (idx + 1) acc t in
  List.rev @@ aux 0 [] l
;;

(* 19. Rotate a list N places to the left. (medium) *)
let rotate l n = 
  let nth = if n >= 0 then n else (-n) in 
  let rec aux idx p s = function 
  | [] -> (List.rev p) @ (List.rev s)
  | h :: t -> if idx >= nth then aux (idx + 1) (h :: p) s t else aux (idx + 1) p (h :: s) t in 
  if n >= 0 then 
    aux 0 [] [] l 
  else List.rev @@ aux 0 [] []  (List.rev l)
;;

(* 20. Remove the K'th element from a list. (easy) *)
(* let remove_at n l = 
  let rec aux idx acc = function 
  | [] -> acc
  | h :: t -> if idx = n then aux (idx + 1) acc t else aux (idx + 1) (h :: acc) t in 
  List.rev @@ aux 0 [] l
;; *)
let rec remove_at n = function 
  | [] -> []
  | h :: t -> if n = 0 then t else h :: remove_at (n - 1) t

(* 21. Insert an element at a given position into a list. (easy) *)
(* let insert_at e n l = 
  let rec aux idx acc = function 
  | [] -> List.rev @@ if idx = n then e::acc  else acc
  | h :: t -> if idx = n then List.rev (h :: e :: acc ) @ t else aux (idx + 1) (h :: acc) t in
  aux 0 [] l
;; *)
let rec insert_at e n = function 
  | [] -> [e]
  | h :: t as l -> if n = 0 then e :: l else h :: insert_at e (n - 1) t

(* 22. Create a list containing all integers within a given range. (easy) *)
let range f s = 
  if f = s then [f]
  else
    let inc = if f < s then 1 else -1 in
    let rec aux cur acc = if cur = s then cur::acc else aux (cur + inc) (cur :: acc) in
    List.rev @@ aux f []
;;

(* 23. Extract a given number of randomly selected elements from a list. (medium) *)
(* let rand_select l n = 
  let len = List.length l in
  let rec aux c acc =  
    if c = n then acc 
    else
    let idx = Random.int len in
    let e = List.nth l idx in
    aux (c + 1) (e :: acc)  in
  List.rev @@ aux 0 [] 
;; *)
let rand_select l n =
  let rec extract acc n = function 
    | [] -> raise Not_found
    | h :: t -> if n = 0 then (h, acc @ t) else extract (h::acc) (n - 1) t in
  let rand_extract l len = extract [] (Random.int len) l in
  let rec aux n acc l len = 
    if n = 0 then acc else 
      let picked, rest = rand_extract l len in
      aux (n - 1) (picked :: acc) rest (len - 1) in
  let len = List.length l in
  aux (min n len) [] l len
  ;;

(* 24. Lotto: Draw N different random numbers from the set 1..M. (easy) *)
let lotto_select n m = 
  let rec aux c acc =
    if c = 0 then acc 
    else 
      let e = Random.int m in
      if List.mem e acc then aux c acc else aux (c - 1) (e :: acc) in
  aux n []
;;  

(* 25. Generate a random permutation of the elements of a list. (easy) *) 
(* TODO:  *)

(* 26. Generate the combinations of K distinct objects chosen from the N elements of a list. (medium) *)
let rec extract k list =
  if k <= 0 then [[]]
  else match list with
    | [] -> []
    | h :: tl ->
      let with_h = List.map (fun l -> h :: l) (extract (k - 1) tl) in
      let without_h = extract k tl in
      with_h @ without_h;