(* heads : int * int list -> int
 * REQUIRES: true
 * ENSURES: heads (x, xs) ==>* the largest n such that the first n elements of xs
 * are all equal to x
 *)
fun heads (x : int, [] : int list) : int = 0
  | heads (x : int, y :: xs : int list) : int =
    if not (x = y) then 0
    else 1 + heads (x, xs)

(* TEST CASES *)
val 0 = heads (1, [])
val 2 = heads (1, [1,1,2,1,3])
val 0 = heads (2, [1,1,2,1,3])
val 5 = heads (1, [1,1,1,1,1])
val 4 = heads (1, [1,1,1,1,5,1,5])


(* tails : int * int list -> int list
 * REQUIRES: true
 * ENSURES: tails (x, xs) ==>* xs with the first k elements removed, where
 * k = heads (x, xs)
 *)
fun tails (x : int, [] : int list) : int list = []
  | tails (x : int, y :: xs : int list) : int list =
  if not (x = y) then y :: xs
  else tails (x, xs)

(* TEST CASES *)
val [] = tails (0, [])
val [1,2,3] = tails (0, [0,1,2,3])
val [2,1,3] = tails (1, [1,1,2,1,3])
val [2,3,1,4] = tails (1, [1,1,1,1,1,1,1,1,2,3,1,4])
val [1,1,2,1,3] = tails (2, [1,1,2,1,3])


(* remove : int * int list -> int list
 * REQUIRES: true
 * ENSURES: remove (x, xs) ==>* xs' such that all x' <> x in xs occur in xs' in
 * the same order as they do in xs and no other elements appear in xs'
 *)
fun remove (x : int, [] : int list) : int list = []
  | remove (x : int, y :: xs : int list) : int list = 
    if x = y then remove (x, xs)
    else y :: remove (x, xs)

(* TEST CASES *)
val [] = remove (100, [])
val [2,3] = remove (1, [1,1,2,1,3])
val [1,1,1,3] = remove (2, [1,1,2,1,3])
val [1,5,1,5,0] = remove (3, [1,5,1,5,0])
val [1,2,3,4,5] = remove (6, [1,2,3,4,5,6])


(* partition : int * int list -> int list * int list
 * REQUIRES: true
 * ENSURES: partition (pivot, L) ==>* (L1, L2) such that
 *          for all x in L1, x <= pivot and
 *          for all y in L2, pivot < y.
 *)
fun partition (pivot : int, [] : int list) : int list * int list = ([],[])
  | partition (pivot : int, x :: xs : int list) : int list * int list =
    let
      val (L1, L2) = partition(pivot, xs)
    in
      if x <= pivot then (x :: L1, L2)
      else (L1, x :: L2)
    end

(* TEST CASES *)
val ([], []) = partition (0, [])
val ([2,2,1,0], [3,5]) = partition (2, [2,2,3,1,0,5])
val ([1,0], [3,4,5]) = partition (2, [1,3,4,0,5])
val ([1,2,3,0], []) = partition (3, [1,2,3,0])
val ([], [11,12,13,200]) = partition (2, [11,12,13,200])

    

