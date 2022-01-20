(* subsetSum : int list * int -> int list option
 * REQUIRES: true
 * ENSURES: subsetSum(L,n) ==>* SOME(L') where L' is a sublist of L which sums
 *          to n. subsetSum(L,n) ==>* NONE if there is no such sublist
 *)
fun subsetSum ([] : int list, 0 : int) : int list option = SOME []
  | subsetSum ([] : int list, n : int) : int list option = NONE
  | subsetSum (x :: xs : int list, n : int) =
        let
            val a = subsetSum (xs, n - x)
        in
            case a of
                NONE => subsetSum (xs, n)
              | SOME y => SOME (x :: y)
        end

val NONE = subsetSum ([1,2], 10)
val SOME [] = subsetSum ([], 0)
val SOME [] = subsetSum ([11, 12], 0)
val SOME [~1, 1] = subsetSum ([~1, 1], 0)
val SOME [1, ~2] = subsetSum ([1, 2, 3, ~2], ~1)
val SOME [3, 5, 7] = subsetSum ([2, 3, 4, 5, 7], 15)