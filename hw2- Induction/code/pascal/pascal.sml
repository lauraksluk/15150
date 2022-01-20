(* pascal : int * int -> int
 * REQUIRES: n >= k >= 0
 * pascal (n, k) ==>* the element of Pascal's triangle at position (n,k)
 *)
fun pascal (n : int, 0 : int) : int = 1
  | pascal (n : int, 1 : int) : int = n
  | pascal (n : int, k : int) : int =
    if n - k = 0 then 1
    else pascal (n - 1, k - 1) + pascal (n - 1, k)

(* TEST CASES *)
val 1 = pascal (0,0)
val 1 = pascal (1,0)
val 1 = pascal (1,1)
val 6 = pascal (6,1)
val 6 = pascal (4,2)
val 1 = pascal (6,6)
val 84 = pascal (9,3)
val 1287 = pascal (13,5)

