print_string("Kevin Williams Exercise 1");;

(*
   These exercises are suggested for programming practice and 
   acclimation to "thinking functionally":
   the way to get better at programming in the 
   functional paradigm is to program in the functional paradigm!
*)

(* 1a. Make it so that that x equals 42, by adding 22 to 20 *)

let x = 20 + 22;;

(* 1b. Make it so that x1 equals 42.0, by casting x. *)

let x1 = float_of_int(x);;



(* 1c. Write a function takes a string, and appends
 * ", and that is why I love functional programming." to the end of it. *)

let loveifier (arg:string)  = arg ^ ", and that is why I love functional programming.";;
loveifier "This is cool";;


(* call your loveifier, creating the mylove string -- yes, this is cheesy ... *)

let mylove:string = loveifier "This is mylove string";;



(* 1d. Write a function that takes a number and returns
 * the difference between that number and 42.
 * Eg, if 'num' is 50, the result should be 8.
 * If 'num' is 30, the result should be -12 *)

let diff_42 num = num - 42;;
diff_42 75;;
diff_42 30;;



(* 1e. One more simple arithmetic example...
 * Write a function that returns the volume of a cylinder
 * with radius r, height h. (volume is pi * r^2 * h) *)
 
let pi = 4.0 *. atan 1.0
let volume_cylinder (r:float) (h:float) : float = 
   pi *. r *. r *. h;;
volume_cylinder 6.0 5.4;;

(*OR*)

let pi = 4.0 *. atan 1.0
let volume_cylinder2 (r:float) (h:float) : float = 
   let r2 = r *. r in
   pi *. r2 *. h;;
volume_cylinder2 6.0 5.4;;



(* 1f. Determine if an integer is even. *)

let even (x: int) : bool = 
      x mod 2 = 0;;
even 115;;
even 116;;


(* 1g. Write odd in terms of even *)

let odd (x: int) : bool = 
   not (even x);;
odd 137;;
odd 148;;


(* 1h. OCaml comes pre-packaged with a standard library, that includes
 * a lot of utility functions that you don't have to write yourself.
 * For instance, check out the String module
 * (http://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html)
 *
 * Now... write a function that takes a String, and returns whether
 * or not that String is more than 10 characters long. *)


let gt_10_chars str : bool = 
 String.length str > 10;;

gt_10_chars "I am the greatest";;
gt_10_chars "Hello";;


(* 2. What does the following expression evaluate to? Why?*)



let why = 1.0 == 1.0;; 
(*
let because = "It evaluates to false because on mutable values x == y iff changing x also changes y,
 in this case we cannot change the variable why and also change 1.0";;  
*)


(* Moral of the story : Don't use == unless you really know what
 * you're doing
 *)


(* 3. Compute the GCD for two integers using Euclid's recursion
 * https://en.wikipedia.org/wiki/Euclidean_algorithm *)

 
let rec gcd (x : int) (y : int) : int = 
   match (x , y) with
   | (x,0) -> x
   | (x,y) -> gcd y ( x mod y)
;;

gcd 1071 462;;


let rec gcd2 (x : int) (y : int) : int = 
    if x = y then x 
    else if x > y then gcd2 (x - y) y
    else gcd2 x (y - x)
   ;;

gcd2 1071 462;;
(* 4. Compute the McCarthy 91 function as shown in 
 * http://en.wikipedia.org/wiki/McCarthy_91_function
 *)



let rec mccarthy (x : int) : int = 
   if x > 100 then (x - 10)
   else mccarthy(mccarthy(x + 11))
;;

mccarthy 109;;



(* 5. Compute the square root of x using Heron of Alexandria's
 * algorithm (circa 100 AD).  x must be greater than 1.0.

 * We start with an initial (poor) guess that the square root is 1.0.
 * Let's call our current guess g.  We'll maintain the invariant that
 * g^2 is less than x and therefore that g is less than the square root
 * of x.

 * Notice that if g is less than the square root of x then x/g is slightly
 * greater than the square root of x.  The real square root is then between
 * g and x/g.

 * To compute a slightly better guess than g, we can take the average of
 * g and x/g:

     g + x/g
     -------
        2

 * We can keep improving our guess by averaging again and again.  Stop
 * the process when you get pretty close.  For this function, when the
 * difference between g and x/g is less than 0.001, compute one final
 * average and return it

 *  More on this method here:

 * http://www.mathpages.com/home/kmath190.htm

 * More on Heron of Alexandria here:

 * http://en.wikipedia.org/wiki/Hero_of_Alexandria
*)

let squareRoot (x : float) : float = 
   let delta = 0.001 in
   let rec guess g =
      let new_g = (g +. (x /. g)) /. 2. in
      if Float.abs (new_g -. g) < delta 
      then new_g else guess new_g
   in
   guess 1.0
;;

squareRoot(144.);;



