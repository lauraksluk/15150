(* add : int * int -> int
 * REQUIRES: n, m >= 0
 * ENSURES: add (n, m) ==>* n + m
 *)
fun add (0 : int, m : int) : int = m
  | add (n : int, m : int) : int = 1 + add (n - 1, m)

(* TEST CASES *)
val 0 = add (0,0)
val 5 = add (0,5)
val 5 = add (5,0)
val 165 = add (15,150)


(* mult : int * int -> int
 * REQUIRES: x, y >= 0
 * ENSURES: mult (x, y) -->* x*y
 *)
fun mult (0 : int, _ : int) : int =  0
  | mult (_ : int, 0 : int) : int = 0
  | mult (x : int, 1 : int) : int = x
  | mult (x : int, y : int) : int = x + mult (x, y - 1)

(* TEST CASES *)
val 0 = mult (0,0)
val 0 = mult (0,5)
val 0 = mult (5,0)
val 10 = mult (5,2)
val 250 = mult (25,10)

(* primeHelp : int * int -> bool
 * REQUIRES: n > 1, c >= 2
 * ENSURES: primeHelp (n, c) -->* true if n is prime and false otherwise
*)
fun primeHelp (2 : int, c : int) : bool = true
  | primeHelp (n : int, c : int) : bool = 
  if n mod c = 0 then false
  else if c = n - 1
    then true
    else primeHelp(n, c + 1)
  
(* TEST CASES *)
val true = primeHelp (2,2)
val true = primeHelp (5,2)
val false = primeHelp (49,2)
val true = primeHelp (53,2)

(* isPrime : int -> bool
 * REQUIRES: n > 1
 * ENSURES: isPrime n ==>* true if n is prime and false otherwise
 *)
fun isPrime (n : int) : bool = primeHelp (n, 2)

(* TEST CASES *)
val true = isPrime (2)
val true = isPrime (3)
val false = isPrime (4)
val true = isPrime (17)
val false = isPrime (15)
val true = isPrime (199)
