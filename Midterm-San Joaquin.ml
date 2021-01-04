(* name: Ayrton San Joaquin
   email address: ayrton@u.yale-nus.edu.sg
   student number: A0190940E

   other members of the group:
   name: Jonathan Chia
   name: Danny Yu Heng
*)

(* The case of the Fibonacci numbers *)
(*Exercise 1*)
let fold_right_nat zero_case succ_case n_given =
 (* fold_right_nat : 'a -> ('a -> 'a) -> int -> 'a *)
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then zero_case
    else let n' = n - 1
         in let ih = visit n'
            in succ_case ih
  in visit n_given;;

  
let fib_v6 n_given =
  let () = assert (n_given >= 0) in
  let (fib_n, fib_succ_n) = fold_right_nat (0,1) (fun (fib_n, fib_succ_n) -> (fib_succ_n, fib_succ_n + fib_n)) n_given
  in fib_n;;


let test_fib candidate =
  (* base cases: *)
let b0 = (candidate 0 = 0)
and b1 = (candidate 1 = 1)
  (* intuitive numbers: *)
and b2 = (candidate 2 = 1)
and b3 = (candidate 3 = 2)
and b4 = (candidate 4 = 3)
and b5 = (candidate 5 = 5)
and b6 = (candidate 6 = 8)
  (* instance of the induction step: *)
and b7 = (let n = Random.int 25
        in candidate n + candidate (n + 1) = candidate (n + 2))
(* etc. *)
in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7;;

let()= assert(test_fib(fib_v6));;


(*A miscellany of recursive programs *)

(*Exercise 3*)
let test_fac candidate =
  (* the base case: *)
let b0 = (candidate 0 = 1)
  (* some intuitive cases: *)
and b1 = (candidate 1 = 1)
and b2 = (candidate 2 = 2)
and b3 = (candidate 3 = 6)
and b4 = (candidate 4 = 24)
and b5 = (candidate 5 = 120)
(* instance of the induction step: *)
and b6 = (let n = Random.int 20
in candidate (succ n) = (succ n) * candidate n)
(* etc. *)
in b0 && b1 && b2 && b3 && b4 && b5 && b6;;



let fac_v3 n_given =
  let () = assert (n_given >= 0) in
  let (i, fac_i)= fold_right_nat (1,1) (fun (i,fac_i)-> ( succ i, i *  fac_i)) n_given
  in fac_i;;

let()= assert(test_fac (fac_v3));;

(*Exercise 4*)

let parafold_right_nat zero_case succ_case n_given =
(* parafold_right_nat : 'a -> (int -> 'a -> 'a) -> int -> 'a *)
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then zero_case
    else let n' = n - 1
        in let ih = visit n'
            in succ_case n' ih    (* <-- succ_case takes two arguments *)
  in visit n_given;;

let test_sumtorial candidate =
  (* the base case: *)
let b0 = (candidate 0 = 0)
  (* some intuitive cases: *)
and b1 = (candidate 1 = 1)
and b2 = (candidate 2 = 3)
and b3 = (candidate 3 = 6)
and b4 = (candidate 4 = 10)
and b5 = (candidate 5 = 15)
  (* instance of the induction step: *)
and b6 = (let n = Random.int 20
        in candidate (succ n) = (succ n) + candidate n)
(* etc. *)
in b0 && b1 && b2 && b3 && b4 && b5 && b6;;


let sumtorial_v0 n_given =
  let () = assert (n_given >= 0) in
  parafold_right_nat 0 (fun n ih -> (succ n) + ih) n_given;;
  
  let()= assert(test_sumtorial (sumtorial_v0))

(*Non-recursive implementation*)

let sumtorial_v1 n_given=
  let() = assert (n_given >= 0) in
((n_given* (n_given+1))/2) ;;


let()= assert(test_sumtorial (sumtorial_v1))




(*Exercise 5*)

(* a & e*)

let test_sum candidate =

  (*For the identity function*)
  (* the base case: *)
let b0 = (candidate (fun x -> x) 0 = 0)
  (* some intuitive cases: *)
and b1 = (candidate (fun x -> x)  1 = 1)
and b2 = (candidate (fun x -> x) 2 = 3)
and b3 = (candidate (fun x -> x) 3 = 6)
and b4 = (candidate (fun x -> x) 4 = 10)
and b5 = (candidate (fun x -> x) 5 = 15)
  (* instance of the induction step: *)
and b6 = (let n = Random.int 20
        in candidate  (fun x -> x) (succ n) = (succ n) + candidate (fun x -> x) n)

      (*For the double function*)
 (* the base case: *)
 and b7 = (candidate (fun x -> 2*x) 0 = 0)
 (* some intuitive cases: *)
and b8 = (candidate (fun x -> 2*x)  1 = 2)
and b9 = (candidate (fun x -> 2*x) 2 = 6)
and b10 = (candidate (fun x -> 2*x) 3 = 12)
and b11 = (candidate (fun x -> 2*x) 4 = 20)
and b12 = (candidate (fun x -> 2*x) 5 = 30)
  (*instance of the induction step: *)
and b13 = (let n = Random.int 20
        in candidate  (fun x -> 2*x) (succ n) = (2* succ n) + candidate (fun x -> 2*x) n)

      (*For the Fibonacci Function*)
 (* the base case: *)
 and b14 = (candidate (fib_v6) 0 +1 = 1)
 (* some intuitive cases: *)
and b15 = (candidate (fib_v6) 1 +1= 2)
and b16 = (candidate (fib_v6) 2 +1= 3)
and b17 = (candidate (fib_v6) 3 +1= 5)
and b18 = (candidate (fib_v6) 4 +1= 8)
and b19 = (candidate (fib_v6) 5 +1= 13)
  (*instance of the induction step: *)
and b20 = (let n = Random.int 20
        in candidate  (fib_v6) (succ n) +1= ( fib_v6 (succ n)) + candidate (fib_v6) n +1)
(* etc. *)
in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 
&& b14 && b15 && b16 && b17 && b18 && b19 && b20;;


(*b*)
(*Recursive sum function*)
let sum_v0 func n_given =
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then func 0
    else let n' = pred n
    in let ih = visit n'
    in func n + ih
    in visit n_given;;
  
let()= assert(test_sum (sum_v0))

(*d*)
(* sum using parafold_right_nat*)
let sum_v1 func n_given =
  let () = assert (n_given >= 0) in
  parafold_right_nat (func 0) (fun n ih ->  (func (succ n)) + ih) n_given;;

let()= assert(test_sum (sum_v1))

(*e*)
(*sumtorial using sum function*)
let sumtorial_v3 n_given =
  sum_v1 (fun x -> x) n_given;;

let()= assert(test_sumtorial (sumtorial_v3))


(*f*)
(*Unit test from week 3*)
let test_sum_odd candidate =
  (candidate 0 = 1)
&& (candidate 1 = 4)
&& (candidate 2 = 9)
&& (candidate 3 = 16)
&& (candidate 4 = 25)
&& (candidate 5 = 36)
&& (candidate 100 = 10201)
(* etc. *);;

(*recursive function*)
let sum_odd_v0  n_given =
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then (fun n -> 2*n +1) 0
    else let n' = pred n
    in let ih = visit n'
    in (fun n' -> 2*n' +1) (succ n') + ih
    in visit n_given;;

let()= assert(test_sum_odd (sum_odd_v0))



(*parafold_right_nat implementation*)
let sum_odd_v1  n_given =
  let () = assert (n_given >= 0) in
  parafold_right_nat ((fun n -> 2*n +1) 0) (fun n' ih ->  ((fun n' -> 2*n' +1) (succ n')) + ih) n_given;;

let()= assert(test_sum_odd (sum_odd_v1))

(*Constant Time*)
let sum_odd_v2 n_given =
  let()=assert (n_given>= 0) in
(n_given + 1) * (n_given +1);; 

let()= assert(test_sum_odd (sum_odd_v2))

(*g*)
(*refer to a*)



(*Exercise 6*)
let fold_right_nat_v1 zero_case succ_case n_given =
  (* fold_right_nat : 'a -> ('a -> 'a) -> int -> 'a *)
   let () = assert (n_given >= 0) in
   parafold_right_nat zero_case (fun n' ih  ->  (succ_case ih)) n_given;;


let parafold_right_nat_v1 zero_case succ_case n_given =
(* parafold_right_nat : 'a -> (int -> 'a -> 'a) -> int -> 'a *)
let () = assert (n_given >= 0) in
  let (index, num)= fold_right_nat (0,zero_case) (fun (index, num) -> (succ index), (succ_case index num)) n_given
  in num;;


let fac_v4 n_given =
let () = assert (n_given >= 0) in
parafold_right_nat_v1 1 (fun n' ih -> (succ n') * ih) n_given;;

let fac_v5 n_given =
let () = assert (n_given >= 0) in
let (i, fac_i)= fold_right_nat_v1 (1,1) (fun (i,fac_i)-> ( succ i, i *  fac_i)) n_given
in fac_i;;

let()= assert(test_fac (fac_v4))

let()= assert(test_fac (fac_v5))



(*Exercise 9*)
(*Individual versions*)

let rec ternary_sing n =
  let()= assert (n>=0) in
  if n =0 
  then true
  else if n=1 then false
  else if n=2 then false
  else ternary_sing (n-3)

let rec preternary_sing n =
  let()= assert (n>=0) in
  if n =2 
  then true
  else if n=1 then false
  else if n=0 then false
  else preternary_sing (n-3)

let rec postternary_sing n =
  if n =1 
  then true
  else if n =0 then false
  else if n=2 then false
  else postternary_sing (n-3);;

(*Mutual Recursion version*)
let rec preternary n =
let () = assert (n >= 0) in
if n= 0
then false 
else let n' = n - 1
      in postternary n'
and  postternary n =
if n= 0
then false 
  else let n' = n - 1
        in ternary n'
and ternary n =
  if n = 0
  then true
  else let n' = n - 1
        in preternary n';;

        
(*Unit tests for negative cases *)

let test_preternaryneg candidate =
  (*testing base case*)
  (candidate 0 = false)

  (*Testing some explicit positive numbers*)
&& (candidate 1 = false)
&& (candidate 3 = false)
&& (candidate 4 = false)
&& (candidate 6 = false)

(* testing the completeness of the preternary predicate in instances where it returns false: *)

&& (let n = Random.int 1000
in candidate (3 * n) = false)
&&  (let n = Random.int 1000
in candidate (3 * n + 1) = false)
(* etc. *);;
let()= assert(test_preternaryneg preternary_sing);;

let()= assert(test_preternaryneg preternary)

let test_postternaryneg candidate =
  (*testing base case*)
  (candidate 0 = false)

  (*Testing some explicit positive numbers*)
&& (candidate 2 = false)
&& (candidate 3 = false)
&& (candidate 5 = false)
&& (candidate 6 = false)

(* testing the completeness of the predicate in instances where it returns false: *)

&& (let n = Random.int 1000
in candidate (3 * n) = false)
&&  (let n = Random.int 1000
in candidate (3 * n -1) = false)
(* etc. *);;

let()= assert(test_postternaryneg postternary_sing);;


let()= assert(test_postternaryneg postternary)

let test_ternaryneg candidate =
  (*testing base case*)
  (candidate 0 = true)

  (*Testing some explicit positive numbers*)
&& (candidate 1 = false)
&& (candidate 2 = false)
&& (candidate 4 = false)
&& (candidate 5 = false)

(* testing the completeness of the predicate in instances where it returns false: *)

&& (let n = Random.int 1000
in candidate (3 * n +1) = false)
&&  (let n = Random.int 1000
in candidate (3 * n -1) = false)
(* etc. *);;

let()= assert(test_ternaryneg ternary_sing);;

let()= assert(test_ternaryneg ternary)

(*unit tests for positive cases*)

let test_preternarypos candidate =
  (*testing base case*)
  (candidate 0 = false)

  (*Testing some explicit positive numbers*)
&& (candidate 2 = true)
&& (candidate 5 = true)
&& (candidate 8 = true)
&& (candidate 11 = true)

(* testing the completeness of the predicate in instances where it returns true: *)

&& (let n = Random.int 1000
in candidate (3 * n -1) = true)
(* etc. *);;

let()= assert(test_preternarypos preternary_sing);;

let()= assert(test_preternarypos preternary)

let test_postternarypos candidate =
  (*testing base case*)
  (candidate 0 = false)

  (*Testing some explicit positive numbers*)
&& (candidate 1 = true)
&& (candidate 4 = true)
&& (candidate 7 = true)
&& (candidate 10 = true)

(* testing the completeness of the predicate in instances where it returns true: *)

&& (let n = Random.int 1000
in candidate (3 * n +1) = true)
(* etc. *);;

let()= assert(test_postternarypos postternary_sing);;

let()= assert(test_postternarypos postternary)

let test_ternarypos candidate =
  (*testing base case*)
  (candidate 0 = true)

  (*Testing some explicit positive numbers*)
&& (candidate 3 = true)
&& (candidate 6 = true)
&& (candidate 9 = true)
&& (candidate 12 = true)

(* testing the completeness of the predicate in instances where it returns true: *)

&& (let n = Random.int 1000
in candidate (3 * n ) = true)
(* etc. *);;

let()= assert(test_ternarypos ternary_sing);;

let()= assert(test_ternarypos ternary)


(*Unit test for ternary predicates (mutual recursion) *)
let test_ternaries postternary ternary preternary =
  (* the base case: *)
let root0 = (ternary 0 = true)
and root1 = (postternary 0 = false)
and root2 = ( preternary 0 = false)
  (* some intuitive cases: *)
and b0 = (preternary 2 = true)
and b1 = ( ternary 3 = true)
and b2 = ( postternary 4 = true)
and b3 = ( preternary 5 = true)
and b4 = ( ternary 6 = true)
and b5 = ( postternary 7 = true)
  (* instance of the induction step: *)
and b6 = (let n = Random.int 1000
        in postternary (succ n) =  ternary n)
and b7 = (let n = Random.int 1000
        in ternary (succ n) = preternary n)
and b8 = (let n = Random.int 1000
        in preternary (succ n) =postternary n)
(* etc. *)
in root0 && root1 && root2 && b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8;;


let()= assert(test_ternaries postternary ternary preternary)


(*Fake functions that pass the unit tests*)
let () =assert (test_preternaryneg (fun n -> if n < 10000 && n mod 3 != 2 then false else true));;
let () =assert (test_postternaryneg (fun n -> if n < 10000 && n mod 3 != 1 then false else true));;
let () =assert (test_ternaryneg (fun n -> if n < 10000 && n mod 3 != 0 then false else true));;

let () =assert (test_postternarypos (fun n -> if n < 10000 && n mod 3 = 1 then true else false));;
let () =assert (test_preternarypos (fun n -> if n < 10000 && n mod 3 = 2 then true else false));;
let () =assert (test_ternarypos (fun n -> if n < 10000 && n mod 3 = 0 then true else false));;

let () =assert (test_ternaries (fun n -> if n < 10000 && n mod 3 = 1 then true else false) 
(fun n -> if n < 10000 && n mod 3 = 0 then true else false) 
(fun n -> if n < 10000 && n mod 3 = 2 then true else false));;


(*The underlying determinism of OCaml*)

let show_bool b =
  (* show_bool : bool -> string *)
   if b
   then "true"
   else "false";;
 
 let show_char c =
  (* show_char : char -> string *)
   "'" ^ (if c = '\\' then "\\\\" else if c = '\'' then "\\\'" else String.make 1 c) ^ "'";;
 
 let show_string s =
  (* show_string : string -> string *)
   "\"" ^ s ^ "\"";;
 
 let show_int n =
  (* show_int : int -> string *)
   if n < 0
   then "(" ^ string_of_int n ^ ")"
   else string_of_int n;;
 
 let show_unit () =
  (* show_unit : unit -> string *)
   "()";;
 
 (* ********** *)
 
 let an_int n =
   let () = Printf.printf "processing %s...\n" (show_int n)
   in n;;
 
 let a_bool b =
   let () = Printf.printf "processing %s...\n" (show_bool b)
   in b;;
 
 let a_char c =
   let () = Printf.printf "processing %s...\n" (show_char c)
   in c;;
 
 let a_string s =
   let () = Printf.printf "processing %s...\n" (show_string s)
   in s;;
 
 let a_unit () =
   let () = Printf.printf "processing the unit value...\n"
   in ();;
 
 let a_function f =
   let () = Printf.printf "processing a function...\n"
   in f;;
 
 (*question 2*)
 
 let f = fun x -> x;;
 
(*a_function (f (an_int 12));;
processing 12...
processing a function...
- : int -> int = <fun>
*)

(*a_function (+) (an_int 1) (an_int 2);;
processing 2...
processing 1...
processing a function...
- : int = 3
*)
 
 (*question 3*)
 
(* let x = a_char 'a' and y = an_int 1 and z = a_string "ASD" and t = a_bool true and u = a_unit () and f = a_function (+);;

processing 'a'...
processing 1...
processing "ASD"...
processing true...
processing the unit value...
processing a function...
val x : char = 'a'
val y : int = 1
val z : string = "ASD"
val t : bool = true
val u : unit = ()
val f : int -> int -> int = <fun>

*)
 (*question 4*)
 
 a_bool true && a_bool false;;
 
 a_bool false && a_bool true;;
 
 a_bool true && a_bool true;;
 
 a_bool false && a_bool false;;
 

 
 (*question 5*)
 
 let random_char () =
   char_of_int (Random.int 32 + 32);;
 
 let test_warmup candidate =
   let b0 = (candidate 'a' 'b' 'c' = "abc")
   and b1 = (candidate 'b' 'c' 'd' = "bcd")
   and b2 = (candidate 'w' 't' 'x' = "wtx")
   and b3 = (candidate '1' '2' '3' = "123")
   and b4 = (candidate '.' '/' '[' = "./[")
   and b5 = (candidate 'a' 'a' 'a' = "aaa")
   and b6 = (candidate '0' '0' '0' = "000")
 in b0 && b1 && b2 && b3 && b4 && b5 && b6;;
 
 let warmup c0 c1 c2 =
   ((String.make 1 c0) ^ (String.make 1 c1) ^ (String.make 1 c2));;
 
 let () = assert (test_warmup warmup);;
 
 (*question 6*) 
 
 let test_map_to_z candidate =
   let b0 = (candidate (fun c -> 'z') "" = "")
   and b1 = (candidate (fun c -> 'z') "x" = "z")
   and b2 = (candidate (fun c -> 'z') "ab" = "zz")
   and b3 = (candidate (fun c -> 'z') "asd" = "zzz")
   and b4 = (candidate (fun c -> 'z') "power" = "zzzzz")
   and b5 = (candidate (fun c -> 'z') "please" = "zzzzzz")
   in b0 && b1 && b2 && b3 && b4 && b5;;
 
 let test_map_to_itself candidate =
   let b0 = (candidate (fun c -> c) "" = "")
   and b1 = (candidate (fun c -> c) "abc" = "abc")
   and b2 = (candidate (fun c -> c) "qwer" = "qwer")
   and b3 = (candidate (fun c -> c) "pore" = "pore")
   and b4 = (candidate (fun c -> c) "123" = "123")
   and b5 = (candidate (fun c -> c) "'[]" = "'[]")
   and b6 = (candidate (fun c -> c) "=-=-" = "=-=-")
 in b0 && b1 && b2 && b3 && b4 && b5 && b6;; 
 
(* String.map (fun c -> a_char c) "adsfsd";;

processing 'a'...
processing 'd'...
processing 's'...
processing 'f'...
processing 's'...
processing 'd'...
- : string = "adsfsd"
*)

let string_map_up f s =
  let n_given = String.length s in
  let rec visit n =
    if n = 0
    then ""
    else let n' = pred n
        in let ih = visit n'
            in ih ^ String.make 1 (f (String.get s n'))
  in visit n_given;;
 

let () = assert (test_map_to_z string_map_up);;

let () = assert (test_map_to_itself string_map_up);;

 (*express in fold_right_nat/parafold*)

let traced_string_map_up f s =
  let n_given = String.length s
    in let rec visit n =
      if n = 0
      then ""
      else let n' = pred n
          in let ih = visit n'
              in ih ^ String.make 1 (f (a_char (String.get s n')))
    in visit n_given;;

 
let string_map_down f s =
  let n_given = String.length s in
    let rec visit n =
    if n = 0
    then ""
    else let n' = pred n
        in let ih = visit n'
            in String.make 1 (f (String.get s (n_given - n))) ^ ih
    in visit n_given;;


 
let () = assert (test_map_to_z string_map_down);;


let () = assert (test_map_to_itself string_map_down);;


let string_map_down_trace f s =
    let n_given = String.length s in
    let rec visit n =
      let result =
    if n = 0
    then ""
    else let n' = pred n
        in let ih = visit n'
            in String.make 1 (f (String.get s (n_given - n))) ^ ih
  in a_string result
    in visit n_given;;

let fold_right_nat zero_case succ_case n_given =
  (* fold_right_nat : 'a -> ('a -> 'a) -> int -> 'a *)
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then zero_case
    else let n' = n - 1
        in let ih = visit n'
            in succ_case ih    (* <-- succ_case takes one argument *)
  in visit n_given;;

let string_map_up_parafold_right_nat f s = 
  parafold_right_nat
    ""
    (fun n ih -> ih ^ String.make 1 (f (String.get s n)))
    (String.length s);;
    
let () = assert (test_map_to_z string_map_up_parafold_right_nat);;

let () = assert (test_map_to_itself string_map_up_parafold_right_nat);;

let string_map_up_fold_right_nat_trace f s = 
  fold_right_nat
    ""
    (fun ih -> let result =
                ih ^ String.make 1 (f (String.get s (String.length ih)))
              in a_string result)
    (String.length s);;

let string_map_down_fold_right_nat f s =
  let n = String.length s in
  fold_right_nat
    ""
    (fun ih -> String.make 1 (f (String.get s (pred n - String.length ih))) ^ ih)
    n;;

let () = assert (test_map_to_z string_map_down_fold_right_nat);;

let () = assert (test_map_to_itself string_map_down_fold_right_nat);;

let string_map_down_fold_right_nat_trace f s =
  let n = String.length s in
  fold_right_nat
    ""
    (fun ih -> let result =
                String.make 1 (f (String.get s (pred n - String.length ih))) ^ ih
              in a_string result)
    n;;

 
 (*Question 7*)
 
(*String.mapi (fun i c -> (a_char (char_of_int (i+48)))) "asdfs";;
processing '0'...
processing '1'...
processing '2'...
processing '3'...
processing '4'...
- : string = "01234"
*)
 
 
 
let test_mapi_to_index_below10 candidate =
  let b0 = (candidate  (fun i c -> char_of_int (i+48)) "" = "")
  and b1 = (candidate (fun i c -> char_of_int (i+48)) "a" = "0")
  and b2 = (candidate (fun i c -> char_of_int (i+48)) "ab" = "01")
  and b3 = (candidate (fun i c -> char_of_int (i+48)) "abc" = "012")
  and b4 = (candidate (fun i c -> char_of_int (i+48))  "abcd" = "0123")
  and b5 = (candidate (fun i c -> char_of_int (i+48))  "abcde" = "01234")
  and b6 = (candidate (fun i c -> char_of_int (i+48))  "qwer" = "0123")
  and b7 = (candidate (fun i c -> char_of_int (i+48))  ",.;'/;" = "012345")
  and b8 = (candidate (fun i c -> char_of_int (i+48))  "abcdef" = "012345")
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8;;

let string_mapi_up f s =
  let n_given = String.length s in
  let rec visit n =
    let result =
      if n = 0
      then ""
      else let n' = pred n
          in let ih = visit n'
              in ih ^ String.make 1 (f (n') (String.get s n'))
    in result
  in visit n_given;;

let string_mapi_down f s =
  let n_given = String.length s in
  let rec visit n =
    let result =
    if n = 0
    then ""
    else let n' = pred n
        in let ih = visit n'
            in String.make 1 (f (n_given - n) (String.get s (n_given - n)))^ ih
in  result
  in visit n_given;;

let()= assert (test_mapi_to_index_below10 string_mapi_up);;   
let()= assert (test_mapi_to_index_below10 string_mapi_down);;   


let parafold_right_nat zero_case succ_case n_given =
  (* parafold_right_nat : 'a -> (int -> 'a -> 'a) -> int -> 'a *)
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then zero_case
    else let n' = n - 1
        in let ih = visit n'
            in succ_case n' ih    (* <-- succ_case takes two arguments *)
  in visit n_given;;

let string_mapi_up_parafold_right_nat f s =
  parafold_right_nat
    ""
    (fun i ih -> let n' = String.length ih in
                ih ^ String.make 1 (f (n') (String.get s (n'))))
    (String.length s);;

let string_mapi_up_parafold_right_nat_trace f s =
  parafold_right_nat
    ""
    (fun i ih -> let result =
                  let n' = String.length ih in
                  ih ^ String.make 1 (f (n') (String.get s (n')))
                in a_string result)
    (String.length s);;

let string_mapi_down_parafold_right_nat f s =
  let n = String.length s in
  parafold_right_nat
    ""
    (fun i ih -> let n' = pred n - String.length ih in
                String.make 1 (f (n') (String.get s (n'))) ^ ih)
    n;;

let string_mapi_down_parafold_right_nat_trace f s =
  let n = String.length s in
  parafold_right_nat
    ""
    (fun i ih -> let result =
                  let n' = pred n - String.length ih in
                  String.make 1 (f (n') (String.get s (n'))) ^ ih
                in a_string result)
    n;;



 (*Question 9a*)
 
(* let x1 = (an_int 1) and x2 = (an_int 2) in (x1,x2);;
processing 2...
processing 1...
- : int * int = (1, 2)
*)

(* let x2 = (an_int 2) and x1 = (an_int 1) in (x1,x2);;

processing 1...
processing 2...
- : int * int = (1, 2)
 *)
 
 (*Question 9b*)
 
(* let x1 = (an_int 1) in let  x2 = (an_int 2) in (x1,x2);;
processing 1...
processing 2...
- : int * int = (1, 2)
*) 

 (*let x2 = (an_int 2) in let x1 = (an_int 1) in (x1,x2);;
processing 2...
processing 1...
- : int * int = (1, 2)
*)
 
 (*Question 10a*)
 
 let x1 = 1 and x2 = 2 in (x1,x2);;
 
 let x2 =2 and x1 =1 in (x1,x2);;
 
 (*Question 10b*)
 
 let x1 = 1 in let x2 = 2 in (x1,x2);;
 
 let x2 = 2 in let x1 = 1 in (x1,x2);;

 


(* Palindromes, string concatenation, and string reversal *)

let random_char ()= char_of_int (Random.int 32+32);;
(* Question 1: String Concatenation with String.init *)

let test_concatenate sc =
  (* associativity*)
  let a0 = (let x = String.make (Random.int  1000) (random_char()) 
            and y = String.make (Random.int  1000) (random_char())
            and z = String.make (Random.int 1000) (random_char()) 
            in sc x(sc y z) = sc (sc x y)z)
  (*identity element*)
  and i0 = (let x = String.make 1000 (random_char())
            and y = ""
            in sc x y = x)
  and i1 = (let x = ""
            and y = String.make (Random.int 1000) (random_char())
            in sc x y = y)
  in a0 && i0 && i1;;

let concatenate s1 s2 = let first = String.length s1 in 
                        let second = String.length s2 in 
                        if first = 0 then s2
                        else if second = 0 then s1 
                        else String.init (first+second) (fun i -> if i < first then s1.[i] else s2.[i - first]);;

let () =assert ( test_concatenate concatenate);;

(* Question 2: String Reversal: mapi *)


(* String Reversal: recursive *)
let string_reverse s =
  let n = String.length s - 1 in String.mapi (fun i c-> String.get s (n - i)) s;;

let string_reverse_rec s=
  let n_given = String.length s in
  let rec visit n =
    if n = 0  then ""
    else let n' = pred n
         in let ih = visit n'
            in ih ^ String.make 1 (s.[n_given - n])
  in visit n_given;;

  (*parafold implementation*)
let string_reverse_parafold s = let n_given = String.length s in 
  let () = assert (n_given >= 0) in
  parafold_right_nat "" (fun n ih -> ih ^ String.make 1 s.[n_given - n - 1]) n_given;;

(* Question 3: Stirng reverse unit test *)
let test_string_reverse candidate =
  let b0 = (candidate "" = "")
  and b1 = (candidate "a" = "a")
  and b2 = (candidate "ab" = "ba")
  and b3 = (candidate "abc" = "cba")
  and b4 = (let a = String.make (Random.int 1000) (random_char()) 
            and b = String.make (Random.int 1000) (random_char())
            in candidate b ^ candidate a = candidate (a ^ b))
  in b0 && b1 && b2 && b3 && b4;;

  

let()=assert(test_string_reverse string_reverse);;
let()=assert(test_string_reverse string_reverse_rec);;
let()=assert(test_string_reverse string_reverse_parafold);;
 
(* Qustion 4: make_palindrome *)

let make_palindrome1 s = let rev = string_reverse s in s ^ rev;;

(* opposite of 1 is also a solution *)

let make_palindrome2 s = let rev = string_reverse s in rev ^ s;;

(* adding one random character in the middle *)

let make_palindrome3 s = let rev = string_reverse s in s ^ String.make 1 (char_of_int (Random.int 32 + 32)) ^ rev;;

let make_palindrome4 s = let rev = string_reverse s in rev ^ String.make 1 (char_of_int (Random.int 32 + 32)) ^ s;;

let make_palindrome5 s = let rev = string_reverse s in let rev_new = String.sub rev 1 (String.length rev - 1) in s ^ rev_new;;

(* Question 5: is_palindrome *)
let test_is_palindrome candidate =
  let b0 = (candidate "" = true)
  and b1 = (candidate (String.make 1 (random_char())) = true)
  and b2 = (candidate (String.make (Random.int 100) (random_char ())) = true)
  and b3 = (candidate "aba" = true)
  and b4 = (candidate "abba" = true)
  and b5 = (candidate "abcba" = true)
  in b0 && b1 && b2 && b3 && b4 && b5;;

(* simple/intuitive approach *)

let is_palindrome s = 
  s = string_reverse s;;

(* with mapi *)

let is_palindrome_mapi s = let n = String.length s -1 in String.mapi (fun i c -> String.get s (n-i)) s = s;;

(* with recursion *)

let is_palindrome_rec s =
  let n_given = String.length s in
  let rec visit n =
    if n = 0
    then true
    else let n' = pred n in
         let ih = visit n'
         in if ih = true
            then String.get s n' = String.get s (n_given - 1 - n')
            else false
  in visit n_given;;

let()=assert(test_is_palindrome is_palindrome);;
let()=assert(test_is_palindrome is_palindrome_mapi);;
let()=assert(test_is_palindrome is_palindrome_rec);;

(*Question 6: Palindrome reversal*)
let rev_pal pal = pal;

(* Question 7: fibonaccize strings *)

(*Unit test*)
let test_fibonaccize candidate =
  let b0 = (candidate "" = " ")
  and b1 = (candidate "ajh" = " jh")
  and b2 = (candidate "iloveocaml" = " lovveeeoooooccccccccaaaaaaaaaaaaammmmmmmmmmmmmmmmmmmmmllllllllllllllllllllllllllllllllll")
  and b3 = (candidate "a" = " ")
  and b4 = (candidate "abba" = " bbaa")
  and b5 = (candidate "abcba" = " bcbbaaa")
  in b0 && b1 && b2 && b3 && b4 && b5;;

let fibonaccize s =
  let n_given = String.length s in
  let rec visit n =
    if n = 0  then " "
    else let n' = pred n
         in let ih = visit n'
            in ih ^ (String.make (fib_v6 n') (String.get s n'))
  in visit n_given;;

let () = assert (test_fibonaccize fibonaccize);;
(* fold_right solutions *)

let fold_right_nat zero_case succ_case n_given =
 (* fold_right_nat : 'a -> ('a -> 'a) -> int -> 'a *)
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then zero_case
    else let n' = n - 1
         in let ih = visit n'
            in succ_case ih
  in visit n_given;;

let parafold_right_nat zero_case succ_case n_given =
(* parafold_right_nat : 'a -> (int -> 'a -> 'a) -> int -> 'a *)
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then zero_case
    else let n' = n - 1
        in let ih = visit n'
            in succ_case n' ih    (* <-- succ_case takes two arguments *)
  in visit n_given;;


let fibonaccize_v2 s = let n_given = String.length s in 
        let () = assert (n_given >= 0) in
        parafold_right_nat " " (fun n ih -> ih ^ (String.make (fib_v6 n) (String.get s n))) n_given;;

let () = assert (test_fibonaccize fibonaccize_v2);;

(*More for the Road*)

(*Exercise 7*)
let string_map func str =
  (* (char -> char ) -> string -> string *)
  String.mapi (fun i c ->func c) str;;

(*Exercise 8*)
let string_mapi func str =
  (* (int -> char -> char ) -> string -> string *)
  String.map (fun c-> c) (String.init (String.length str) (fun i-> func i str.[i]));;
