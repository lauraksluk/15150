(* findPartitionV1 :
 *  'e list ->
 *  ('e list -> 'l option) ->
 *  ('e list -> 'r option) ->
 *  ('l * 'r) option
 * REQUIRES: pL and pR are total
 * ENSURES: findPartitionV1 A pL pR ==>*
 *  SOME (LL, RR) if there exists a partition (L, R) of A
 *  such that pL accepts L with LL and pR accepts R with RR.
 *  NONE otherwise.
 *)
fun findPartitionV1 ([] : 'e list) (pL : 'e list -> 'l option) 
                    (pR : 'e list -> 'r option) : ('l * 'r) option =
                      (case pL([]) of
                        SOME left => (case pR([]) of
                                        SOME right => SOME (left, right)
                                      | NONE => NONE)
                      | NONE => NONE)
  | findPartitionV1 (x :: xs) pL pR : ('l * 'r) option =
      let
        val left = findPartitionV1 xs (fn l => pL(x :: l)) pR
        val right = findPartitionV1 xs pL (fn r => pR(x :: r))
      in
        case left of
          SOME (L, R) => SOME (L, R)
        | NONE => case right of 
                    SOME (L, R) => SOME (L, R)
                  | NONE => NONE
      end

(* TEST CASES *)
fun pL [] = SOME 0 | pL (x::xs) = if x mod 2 = 0
                                  then case (pL xs) of
                                          SOME y => SOME (y + 1)
                                        | NONE => NONE
                                  else NONE
fun pR [] = SOME 0 | pR (x::xs) = if x mod 2 = 1
                                  then case (pR xs) of
                                          SOME y => SOME (y + 1)
                                        | NONE => NONE
                                  else NONE    
fun pL1 [] = SOME 0 | pL1 (x::xs) = if x mod 3 = 0
                                    then case (pL xs) of
                                            SOME y => SOME (y + 1)
                                          | NONE => NONE
                                    else NONE
val SOME (1,2) = findPartitionV1 [1,2,3] pL pR       
val SOME (0,3) = findPartitionV1 [1,5,3] pL pR
val NONE = findPartitionV1 [1,1,1] pL1 pL

(* findPartitionV2 :
 *  'e list ->
 *  ('e list -> 'l option) ->
 *  ('e list -> 'r option) ->
 *  ('l * 'r -> 'a) ->
 *  (unit -> 'a) ->
 *  'a
 * REQUIRES: pL and pR are total
 * ENSURES: findPartitionV2 A pL pR s k ==>*
 *  s (LL, RR) if there exists a partition (L, R) of A
 *  such that pL accepts L with LL and pR accepts R with RR.
 *  k () otherwise.
 *)
fun findPartitionV2 ([] : 'e list) (pL : 'e list -> 'l option) 
                    (pR : 'e list -> 'r option) (sc : 'l * 'r -> 'a)
                    (fc : unit -> 'a) : 'a = 
                      (case pL([]) of
                        SOME left => (case pR([]) of
                                      SOME right => sc(left, right)
                                    | NONE => fc())
                      | NONE => fc())
  | findPartitionV2 (x :: xs) pL pR sc fc =
      findPartitionV2 xs (fn l => pL(x :: l)) pR sc 
                         (fn () => findPartitionV2 xs pL 
                         (fn r => pR(x :: r)) sc fc)

(* TEST CASES *)
val sc = SOME
val fc = fn () => NONE
val SOME (1,2) = findPartitionV2 [1,2,3] pL pR sc fc

(* findPartition :
 *  'e list ->
 *  ('e list -> ('l -> 'a) -> (unit -> 'a) -> 'a) ->
 *  ('e list -> ('r -> 'a) -> (unit -> 'a) -> 'a) ->
 *  ('l * 'r -> 'a) ->
 *  (unit -> 'a) ->
 *  'a
 * REQUIRES: pL and pR are CPS-total
 * ENSURES: findPartition A pL pR sc fc ==>
 *  sc (LL, RR) if there exists a partition (L, R) of A
 *  such that pL accepts L with LL and pR accepts R with RR.
 *  fc () otherwise.
 *)
fun findPartition ([] : 'e list) 
                  (pL : 'e list -> ('l -> 'a) -> (unit -> 'a) -> 'a)
                  (pR : 'e list -> ('r -> 'a) -> (unit -> 'a) -> 'a)
                  (sc : 'l * 'r -> 'a) (fc : unit -> 'a) : 'a =
                    pL [] (fn l => pR [] (fn r => sc(l, r)) fc) fc
  | findPartition (x :: xs) pL pR sc fc =
      findPartition xs (fn l => pL(x :: l)) pR sc 
                       (fn () => findPartition xs pL
                       (fn r => pR(x :: r)) sc fc)

(* TEST CASES *)

val sum = foldr op+ 0

fun subsetSum (n : int) (xs : int list) : int list option =
  let
    fun pL L scL fcL = if sum L = n then scL L else fcL ()
    fun pR R scR fcR = scR ()
    fun sc (LJ, ()) = SOME LJ
    fun fc () = NONE
  in
    findPartition xs pL pR sc fc
  end

val SOME [] = subsetSum 0 [] 
val NONE = subsetSum 1 []
val SOME [1] = subsetSum 1 [1]
val SOME [1,1] = subsetSum 2 [1,1]
val SOME [1,2] = subsetSum 3 [1,2,1]
val SOME [2,1] = subsetSum 3 [5,2,1]
val NONE = subsetSum 3 [0,0,0]
val SOME [2,2] = subsetSum 4 [2,2,2]