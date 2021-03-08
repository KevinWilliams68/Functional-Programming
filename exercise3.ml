(* Goal: more practice with OCaml. In particular, we'll be
   starting to see the power of higher order functions.

   Topics:
   * Map and reduce
   * fold_left, fold_right
*)

(* Problem 0 : Some auxiliary functions we may use later from Exercise 2
 * If you did them last week, then you can just copy them over. Otherwise,
 * it'll be good review practice.
*)

(* 0a. Write a higher-order function for binary operations on options.
 * If both arguments are None, return None.  If one argument is (Some x)
 * and the other argument is None, function should return (Some x) *)

(* What is calc_option's function signature? *)


let co_sig:string = "It is, ('a -> 'a -> 'a) -> 'a option -> 'a option";;



let calc_option (f: 'a->'a->'a) (x: 'a option) (y: 'a option) : 'a option =
  match (x, y) with
  | (None, None) -> None
  | (Some a, None) -> Some a
  | (None, Some b) -> Some b 
  | (Some a, Some b) -> Some (f a b)
;; 


(* 0b. Now rewrite the following functions using the above higher-order function.
 * Write a function to return the smaller of two int options, or None
 * if both are None. If exactly one argument is None, return the other. *)


let min_option (x: int option) (y: int option) : int option =
  calc_option min x y;;


let max_option (x: int option) (y: int option) : int option =
  calc_option max x y;;

(* 0c. Write a function to return the boolean AND/OR of two bool options,
 * or None if both are None. If exactly one is None, return the other. *)


let and_option (x:bool option) (y: bool option) : bool option =
  match (x,y) with
  | (None, None) -> None
  | ( a, None) -> a
  | (None,  b) -> b
  | (Some a, Some b) -> Some (a && b)
;;

let or_option (x:bool option) (y: bool option) : bool option =
  match (x, y) with
  | (None, None) -> None 
  | ( a, None) -> a
  | (None, b) -> b
  | (Some a, Some b) -> Some (a || b)
;;

(* Problem 1 *)
(* Map and reduce are the functions we've seen before, with
 * arguments in a sane order to be partially defined  *)
let rec reduce (f:'a -> 'b -> 'b) (u:'b) (xs:'a list) : 'b =
  match xs with
  | [] -> u
  | hd::tl -> f hd (reduce f u tl)
;;

let rec map (f:'a -> 'b) (xs: 'a list) : 'b list =
  match xs with
  | [] -> []
  | hd::tl -> (f hd) :: (map f tl)
;;

(* 1a. Implement length in terms of reduce.
 * length lst returns the length of lst. length [] = 0. *)

let length (lst: int list) : int =
  let rec count n = function 
    | [] -> 0
    | _::tl -> count (n + 1) tl 
in count 0 lst
;; 




(* 1b. Write a function that takes an int list and multiplies every int by 3.
 * Use map. *)

let times_3 (lst: int list): int list =
   map (fun x -> x * 3) lst
;;
(*
times_3 [5;6;7;8;9];;
*)

(* 1c. Write a function that takes an int list and an int and multiplies every
 * entry in the list by the int. Use map. *)

let times_x (x: int) (lst: int list) : int list =
  map (fun l -> x * l) lst
;;
(*
times_x 5 [1;2;3;4;5];;
*)

(* 1d. Rewrite times_3 in terms of times_x.
 * This should take very little code. *)

let times_3_shorter =
  times_x 3;;

(* 1e. Write a function that takes an int list and generates a "multiplication
 * table", a list of int lists showing the product of any two entries in the
 * list. e.g. mult_table [1;2;3] => [[1; 2; 3]; [2; 4; 6]; [3; 6; 9]] *)

let mult_table (lst: int list) : int list list =
  map (fun x -> times_x x lst) lst
;;



(* 1f. Write a function that takes a list of boolean values
 * [x1; x2; ... ; xn] and returns x1 AND x2 AND ... AND xn.
 * For simplicity, assume and_list [] is TRUE. Use reduce. *)

let and_list (lst: bool list) : bool =
  reduce (fun x acc -> x && acc) true lst
;;



(* 1g. Do the same as above, with OR.
 * Assume or_list [] is FALSE. *)

let or_list (lst: bool list) : bool =
  reduce (fun x acc -> x && acc) false lst
;;


(* 1h.	 Write a function that takes a bool list list and returns
 * its value as a boolean expression in conjunctive normal form (CNF).
 * A CNF expression is represented as a series of OR expressions joined
 * together by AND.
 * e.g. transform this:

   [ [x1; x2]; [x3; x4; x5]; [x6] ]

   in to this:

   (x1 OR x2) AND (x3 OR x4 OR x5) AND (x6).

 * Use map and/or reduce.
 * You may find it helpful to use and_list and or_list. *)


let cnf_list (lst: bool list list) : bool =
  reduce (fun x acc -> or_list(x) && acc) true lst
;;
(*
cnf_list [ [true; false]; [true; false; false]; [true] ];;
*)

(* You may use a function you wrote above to solve questions 1i to 1k. *)

(* 1i. Write and_list to return a bool option,
 * where the empty list yields None. Use map and/or reduce.
*)

let and_list_smarter (lst: bool list) : bool option =
  reduce and_option None (map (fun x -> Some x) lst)
;;

(* 1j. Return the max of a list, or None if the list is empty. *)

let max_of_list (lst:int list) : int option =
  match lst with
  | [] -> None
  | hd :: _ -> Some (reduce (fun x acc -> if x > acc then x else acc) hd lst)
;;    

(* 1k. Return the min and max of a list, or None if the list is empty. *)

let bounds (lst:int list) : (int option * int option)  =
  match lst with 
  | [] -> (None, None) 
  | hd::_ -> 
    let max = max_of_list lst in
    let min = Some (reduce (fun x acc -> if x < acc then x else acc) hd lst) in
    (min,max)
;;

(* Problem 2 : Folds *)
(* equivalent to List.fold_right in the List module *)
let rec fold_right (f:'a -> 'b -> 'b) (xs:'a list) (base:'b) : 'b =
  match xs with
    [] -> base
  | hd::tail -> f hd (fold_right f tail base)
;;

(* equivalent to List.fold_left in the List module *)
(* more efficient than fold_right (tail recursive) *)
let rec fold_left (f:'b -> 'a -> 'b) (base:'b) (xs:'a list) : 'b =
  match xs with
    [] -> base
  | hd::tail -> fold_left f (f base hd) tail
;;
(* 2a. Write reduce from lecture in terms of a fold operation *)

let reduce f base xs = fold_right f xs base;;

(* 2b. Takes a list and returns the same list: use a fold *)

let id xs = fold_right (fun x acc -> x::acc) xs [];;


(* 2c. takes a list and returns the reverse: use a fold *)

let reverse xs = fold_left (fun acc x -> x::acc) [] xs;;


(* 2d. write the polymorphic map from lecture using fold *)

let map f xs = fold_right (fun x acc -> x::acc) xs [];;


(* 2e. write a function that maps f across a list and reverses the result *)

let map_rev f xs = fold_right (fun x acc -> f acc::x) [] xs;;


(* 2f. Write a function that concats two lists into a single list
 *  not using the @ operator *)

(* With recursion *)

let rec concat xs ys =
  match xs with
  | [] -> ys 
  | hd::tl -> hd::concat tl ys
;;

(* Without recursion, using fold *)

let concatF xs ys = fold_right (fun x y -> x::y) xs ys;;
