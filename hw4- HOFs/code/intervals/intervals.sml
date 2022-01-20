type interval = int * int

(* cmp : interval * interval -> order
 * REQUIRES: interval1, interval2 are well-formatted time intervals
 * ENSURES: cmp (interval1, interval2) => LESS if the first tuple starts
 *                                        or ends at an earlier time,
 *          cmp (interval1, interval2) => GREATER if the first tuple starts
 *                                        or ends at a later time,
 *          cmp (interval1, interval2) => EQUAL if the tuples are equal
 *
 *)
fun cmp ((start1, end1), (start2, end2)) : order =
    case Int.compare (start1, start2) of 
        LESS => LESS
      | GREATER => GREATER
      | _ => Int.compare (end1, end2)

(* TEST CASES *)
val GREATER = cmp ((4,6), (1,2))
val GREATER = cmp ((1,3), (1,2))
val LESS = cmp ((1,3), (5,6))
val LESS = cmp ((2,4), (2,5))
val EQUAL = cmp ((1,9), (1,9))


(* free_times: interval * (interval list * int) -> interval list * int
 * REQUIRES: curr_interv is a well-formatted time interval
 * ENSURES: free_times (curr_interv, (L, end)) => (L', end') where L' is
 *          a well-formatted list of intervals and end' is the current
 *          end time for comparison.
 *)
fun free_times ((startT, endT), (curr_end, result)) : int * interval list =
    case curr_end < startT of
        true => (endT, (curr_end, startT) :: result)
      | false => (endT, result)
  
(* TEST CASES *)
val (2, [(0,1)]) = free_times ((1,2), (0, []))
val (9, [(0,1)]) = free_times ((5,9), (6, [(0,1)]))


(* all_available: interval list list -> interval list
 * REQUIRES: B is a well-formatted list of staff's busy time intervals
 * ENSURES: all_available(B) => L, where L is a list of well-formatted
 *         time intervals where everyone is free.
 *         Implementation runs in O(nlog n) time.
 *)
fun all_available ([] : interval list list) : interval list = []
  | all_available (B: interval list list) : interval list = 
    let
      val sorted_B = msort cmp (List.concat B)
      val (curr_end, combine) = foldl free_times (0, []) sorted_B
    in
      combine
    end

(* TEST CASES *)
val [] = all_available []
val [(3,4), (0,1)] = all_available [[(1,2), (4,6), (8,12)], [(1,3), (5,9)]]
val [(3,5), (1,2)] = all_available [[(0,1), (2,3)], [(5,7)]]
val [(1,2)] = all_available [[(0,1), (2,3)]]
