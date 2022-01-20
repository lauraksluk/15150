(* populate: bool * bool list list -> bool list list
 * REQUIRES: true
 * ENSURES: populates each inner list within 2-d list argument with b
*)
fun populate (b : bool, [] : bool list list) : bool list list = []
  | populate (b : bool , x :: xs : bool list list) : bool list list =
    (b :: x) :: populate (b, xs)

(* TEST CASES *)
val [] = populate (true, [])
val [[false, true], [false, true]] = populate (false, [[true], [true]])
val [[true, false], [true]] = populate (true, [[false], []])


(* flipOne: bool list -> bool list list
 * REQUIRES: true, 
 * ENSURES: flipOne L ==>* An ordered list containing all the ways to flip exactly
 *          one true to false.
 *)
fun flipOne ([] : bool list) : bool list list = []
  | flipOne ([true] : bool list) : bool list list = [[false]]
  | flipOne ([false] : bool list) : bool list list = []
  | flipOne (x :: xs : bool list) : bool list list =
    if x
    then (false :: xs) :: populate (true, flipOne(xs))
    else populate (false, flipOne(xs))
  
(* TEST CASES *)
(* Write test cases here *)
val [] = flipOne []
val [[false, true], [true, false]] = flipOne [true, true]
val [[false, false ]] = flipOne [false, true]
