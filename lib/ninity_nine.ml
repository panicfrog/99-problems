
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
