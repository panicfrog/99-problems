
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
    | h :: t -> if i = n then aux (i + 1) acc t else aux (i + 1) (h :: acc) t in
  List.rev @@ aux 1 [] l 

  