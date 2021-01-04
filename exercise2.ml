
fun x -> fun a-> fun b-> if x then a else b;;


(*if is a boolean 
 then & else must have same type !!!*)


fun x -> fun a-> fun b-> if x then b else a;;



 (*2.c fun z -> fun a -> fun b-> z a b;;
 knows z takes in a, b and then outputs something (we denote c) which is same for the end.
*)
 (*2.d*)

 fun z -> fun b -> fun a-> z a b;;

 fun z -> fun a -> fun b-> z a b;;

 fun x -> fun b -> fun a -> (x a, x b);;


 (*Exercise 1*)
(*A*) fun a -> fun b-> (a,b);;
(*B*) fun a -> fun b -> (b,a);;
(*C.1*) fun x -> fun a-> fun b-> if x then a else b;;
(*C.2*) fun x -> fun a-> fun b-> if x then b else a;;

(*Exercise 2*)

(*A*) fun a -> (a,a);;
(*B*) fun a -> a ();;
(*C*) fun z -> fun a -> fun b-> z a b;;
(*D*) fun z -> fun b -> fun a-> z a b;; 
(*E*) fun x -> fun b -> fun a -> (x a, x b);;
(*F*) fun z -> fun a -> fun b -> z (a,b);;
(*G*) fun x -> fun y -> fun g -> (y(x g));;

