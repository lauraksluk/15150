
type 'a ord = 'a * 'a -> order

(* cmpIsIn : 'a ord * 'a * 'a list -> bool
 * REQUIRES: cmp is an order function
 * ENSURES: cmpIsIn (cmp, x, L) ==> true if x is cmp-in L
 *          cmpIsIn (cmp, x, L) ==> false otherwise
 *)
fun cmpIsIn (cmp : 'a ord, x : 'a, [] : 'a list) = false
  | cmpIsIn (cmp : 'a ord, x : 'a, y :: L : 'a list) =
    if cmp (x, y) = EQUAL then true
    else cmpIsIn (cmp, x, L)


(* TEST CASES *)
val L = [4, ~2, 6, 4, ~3, 2, 6, 7]
val cmp = Int.compare

val false = cmpIsIn (cmp, 6, [])
val true = cmpIsIn (cmp, 6, L)
val false = cmpIsIn (cmp, 3, L)
val true = cmpIsIn (cmp, ~3, L)


(* slowDoop : 'a ord * 'a list -> 'a list
 * REQUIRES: cmp is an order function
 * ENSURES: slowDoop (cmp, L) ==> L', where L' is L with 
 *          the cmp-duplicates removed
 *)
fun slowDoop (cmp : 'a ord, [] : 'a list) = []
  | slowDoop (cmp : 'a ord, x :: L : 'a list) = 
    case (cmpIsIn (cmp, x, L)) of 
        true => slowDoop (cmp, L)
      | false => x :: (slowDoop (cmp, L))


(* TEST CASES *)
val cmp = Int.compare

val L = [4, ~2, 6, 4, ~3, 2, 6, 7]
val [~2, 4, ~3, 2, 6, 7] = slowDoop (cmp, L)

val L = [4, 4, 2, 7, 2]
val [4, 7, 2] = slowDoop (cmp, L)
