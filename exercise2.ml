(*Kevin Williams*)
(* Goal: more practice with Ocaml. In particular, we'll be
   starting to see the power of higher order functions.  

  Topics:
   * records, tuples and lists
   * options
   * higher order functions

*)

type employee = {name:string ;  married:bool; age:int}
(* 0a. Make a function that makes a string * int * bool into an employee and vice versa *)

let employee_of_tuple (t: string * bool * int) : employee =
   let (name, married, age) = t in 
   {name = name; married = married; age = age}
;;  

let tuple_of_employee (e:employee) : (string * bool * int) =
   (e.name, e.married, e.age)
;;    

(*
Check functions 0a

let k : employee = employee_of_tuple ("kevin", false, 28);;
tuple_of_employee k;;
*)


(* 0b. Why doesn't the expression below type check? How can you make
 * it typecheck with one small change to the declaration of get_name? *)

 
let why = "Ocaml compiler thought get_name was of type 'employer' and could not be used with type employee";;
let how = "We have to tell Ocaml that 'e' is of type employee by decalring its type";;
type employer = {name:string; est_year:int };;
let get_name (e:employee) = e.name;;
let g : employee = employee_of_tuple ("Greg", true, 12);;
let _ = print_string (get_name g);;

(*
let why = ""
let how = ""
type employer = {name:string; est_year:int }
let get_name e = e.name
let g : employee = employee_of_tuple ("Greg", true, 12)
let _ = print_string (get_name g)
*)


(* 0c. Reimplement the OCaml standard functions List.length and List.rev *)


let length (l:'a list) : int = 
   let rec count n = function
      | [] -> 0
      | _ ::tl -> count (n + 1) tl
   in count 0 l
;;   

let rev (l:'a list) : 'a list = 
   let rec aux reverse = function
      | [] -> reverse
      | hd::tl -> aux (hd::reverse) tl in
   aux [] l
;;


(* 0d. Remove the kth element from a list. Assume indexing from 0 *)
(* example: rmk 2 ["to" ; "be" ; "or" ; "not" ; "to" ; "be"] 
 * results in: [["to" ; "be" ; "not" ; "to" ; "be"] *)
 let rec rmk (k:int) (l:'a list) : 'a list =  
   match l with
   | [] -> []
   | hd :: tl -> 
      if (k = 0) then tl
      else hd :: (rmk (k - 1) tl)
;;


(*
rmk 2 ["to" ; "be" ; "or" ; "not" ; "to" ; "be"] ;;
*)


(* Exercise 1 : Options and Options in functions *)
(* 1a. Write a function to return the smaller of two int options, or None
 * if both are None. If exactly one argument is None, return the other. Do 
 * the same for the larger of two int options.*)


let min_option (x: int option) (y: int option) : int option = 
   match (x, y) with
   | (None, None) -> None
   | (x, None) -> x
   | (None, y) -> y
   | (x,y) -> if x > y then y else x
;;
(*
min_option (Some 5) (Some 7);;
*)
let max_option (x: int option) (y: int option) : int option = 
   match (x, y) with
   | (None, None) -> None
   | (x, None) -> x
   | (None, y) -> y
   | (x,y) -> if x > y then x else y
;;

(*
max_option (Some 5) (Some 7);;
*)


(* 1b. Write a function that returns the integer buried in the argument
 * or None otherwise *)  
 
let get_option (x: int option option option option) : int option = 
   match x with
   | None -> None
   | Some None -> None
   | Some (Some None) -> None
   | Some (Some (Some None)) -> None
   | Some (Some (Some (Some a))) -> Some a
;;

(*
get_option (Some (Some (Some (Some 5))));;
*)


(* 1c. Write a function to return the boolean AND/OR of two bool options,
 * or None if both are None. If exactly one is None, return the other. *)


let and_option (x:bool option) (y: bool option) : bool option = 
   match (x, y) with 
   | (None, None) -> None
   | (a , None) ->  a 
   | (None, a) ->  a 
   | (Some a, Some b) -> Some (a && b)
;;


let or_option (x:bool option) (y: bool option) : bool option = 
   match (x, y) with 
   | (None, None) -> None
   | (a , None) ->  a 
   | (None, a) ->  a 
   | (Some a, Some b) -> Some (a || b)
;;

(* What's the pattern? How can we factor out similar code? *)

(* 1d. Write a higher-order function for binary operations on options.
 * If both arguments are None, return None.  If one argument is (Some x)
 * and the other argument is None, function should return (Some x) *)
(* What is calc_option's function signature? *)


let calc_option (f: 'a->'a->'a) (x: 'a option) (y: 'a option) : 'a option =  
   match (x, y) with
   | (None, None) -> None
   | (Some a, None) -> Some a
   | (None, Some b) -> Some b 
   | (Some a, Some b) -> Some (f a b)
;; 

(* 1e. Now rewrite the following functions using the above higher-order function
 * Write a function to return the smaller of two int options, or None
 * if both are None. If exactly one argument is None, return the other. *)


let min_option2 (x: int option) (y: int option) : int option = 
   calc_option min x y;;




let max_option2 (x: int option) (y: int option) : int option = 
   calc_option max x y;;

(*
min_option2 (Some 5) (Some 7);;
max_option2 (Some 5) (Some 7);;
*)

(* 1f. Write a function that returns the final element of a list, 
   if it exists, and None otherwise *)

let rec final (l: 'a list) : 'a option = 
   match l with
   | [] -> None
   | [x] -> Some x
   | _::tl -> final tl
;;
(*
final [ "a" ; "b" ; "c" ; "d" ];;
*)