(* sum : int list -> int
 * REQUIRES: true
 * ENSURES: sum [x1, ..., xn] = x1 + ... + xn
 *)
val sum : int list -> int = foldr op+ 0


(* won'tStarve :
 *   int ->
 *   int ->
 *   int list ->
 *   (int list list -> 'a) ->
 *   (unit -> 'a) ->
 *   'a
 * REQUIRES: pots >= 0 and time >= 0 and for all x in curries, x > 0
 * ENSURES: won'tStarve pots time curries sc fc ==>
 *  sc D if there exists D such that concat D is a permutation of curries
 *  and length D = pots and for all d in D, sum d <= time.
 *  fc () otherwise.
 *)
fun won'tStarve (0 : int) (time : int) (curries : int list) 
                (sc : int list list -> 'a) (fc : unit -> 'a) : 'a =
                  if curries = [] then sc [] else fc ()
  | won'tStarve pots time curries sc fc =
      let
        fun pL left scL fcL = if sum(left) <= time 
                              then scL(left) else fcL ()
        fun pR right scR fcR = won'tStarve (pots - 1) time right scR fcR 
      in
        findPartition curries pL pR (fn (l, r) => sc(l :: r)) fc
      end

(* TEST CASES *)
val SOME [] = won'tStarve 0 50 [] SOME (Fn.const NONE)
val NONE = won'tStarve 0 50 [10] SOME (Fn.const NONE)
val SOME [[10], [], []] = won'tStarve 3 50 [10] SOME (Fn.const NONE)
val SOME [[10,20], [30], [40]] = won'tStarve 3 50 [10, 20, 30, 40] SOME (Fn.const NONE)
val SOME [[5,10], [10], [20]] = won'tStarve 3 20 [5, 10, 10, 20] SOME (Fn.const NONE)
val NONE = won'tStarve 2 40 [20, 20, 30, 40] SOME (Fn.const NONE)
