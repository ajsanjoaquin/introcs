(*Exercise 5*)

let show_int n =
  (* show_int : int -> string *)
   if n < 0
   then "(" ^ string_of_int n ^ ")"
   else string_of_int n;;

let show_bool b =
(* show_bool : bool -> string *)
  if b
  then "true"
  else "false";;

let show_string s =
  (* show_string : string -> string *)
    "\"" ^ s ^ "\"";;

let show_char c =
  (* show_char : char -> string *)
    "'" ^ String.make 1 c ^ "'";;

let show_quadruple show_yourself1 show_yourself2 show_yourself3 show_yourself4 (v1, v2, v3,v4) =
 (* show_quadruple : ('a -> string) -> ('b -> string) -> ('c -> string) -> ('d -> string) -> 'a * 'b * 'c * 'd -> string *)
  "(" ^ (show_yourself1 v1) ^ ", " ^ (show_yourself2 v2) ^ ", " ^ (show_yourself3 v3) ^ ", " ^ (show_yourself4 v4) ^ ")";;

(*Unit tests and  assertions*)
let test_show_int_cross_bool_cross_int_cross_string candidate =
  let b0 = (candidate (432, false, 1, "Shroud of Turing") = "(432, false, 1, \"Shroud of Turing\")")
  and b1 = (candidate (164, true, 3204509, "Test of Man") = "(164, true, 3204509, \"Test of Man\")")
  in b0 && b1;;

let test_show_char_cross_int_cross_string_cross_bool candidate =
  let b0 = (candidate ('~', 1329, "i can write", true) = "('~', 1329, \"i can write\", true)")
  and b1 = (candidate ('\\', 123, "i hope not to redo", false) = "('\\', 123, \"i hope not to redo\", false)")
  in b0 && b1;;

let()=assert(test_show_int_cross_bool_cross_int_cross_string (show_quadruple show_int show_bool show_int show_string) );;

let()=assert(test_show_char_cross_int_cross_string_cross_bool (show_quadruple show_char show_int show_string show_bool));;

(*Nested quadruple*)
show_quadruple (show_quadruple show_int show_bool show_int show_string) show_bool show_int show_string ((432, false, 1, "Shroud of Turing"), false, 1, "Shroud of Turing");;



(*Exercise 9*)

(*Unit test*)
let test_oddp candidate =

(*Testing coverage for negative numbers*)
  let mgh1= (candidate (-5) = true)

and mgh2= (candidate (-14) =false)

and mgh3 = (candidate (-1) = true)

  (* testing base case *)
and z = (candidate 0 = false)

(*Testing some explicit positive numbers*)
and hard1=(candidate 5 =true)

and hard2=(candidate 16 = false)

and hard3=(candidate 14 = false)
    (* testing the completeness of the odd predicate: *)

and com1 = (let n = Random.int 1000
          in candidate (2 * n) = false)
and com2 = (let n = Random.int 1000
          in candidate (2 * n + 1) = true)
and com3 =(let n= (Random.int 1000)*(-1) 
          in candidate (2*n) = false)
and com4 = (let n=(Random.int 1000)*(-1) 
          in candidate (2*n +1) = true)

    (* an instance of the induction step: *)
and is = (let n' = Random.int 1000
          in candidate (succ n') = not (candidate n'))

in mgh1 && mgh2 && mgh3 && z && hard1 && hard2 && hard3 && com1 && com2 && com3 && com4 && is;;


 (*Taken from Even predicate section of Week 5 Lecture Notes*) 
let evenp_v1 n_given =
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then true
    else not (visit (pred n))
  in visit n_given;;
  
(*our odd predicate function*)
let oddp n =
  if n>0
  then 
  not(evenp_v1 n)
  else
  not(evenp_v1 (-n));;


(*Fake function*)
test_oddp (fun n -> if n < 10000 && n mod 2 == 0 then false else true);;