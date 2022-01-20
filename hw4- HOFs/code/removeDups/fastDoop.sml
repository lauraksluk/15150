
type 'a ord = 'a * 'a -> order

(* THE FOLLOWING 3 FUNCTIONS IMPLEMENT MSORT, A.K.A MERGESORT.
 * YOU WILL NEED MSORT FOR YOUR IMPLEMENTATION OF FASTDOOP *)

(* split : 'a list -> 'a list * 'a list
 * REQUIRES: true
 * ENSURES: split L ==> (l,r) where l @ r is a permutation of L
 *)
fun split ([] : 'a list) : 'a list * 'a list = ([],[])
  | split [x] = ([x],[])
  | split (x::y::L) =
      let
        val (A,B) = split L
      in
        (x::A, y::B)
      end

(* merge : 'a ord * 'a list * 'a list -> 'a list
 * REQUIRES: cmp is an order function, l and r are sorted
 * ENSURES: merge (cmp, l, r) ==> L, where L is a cmp-sorted permutation of l @ r
 *)
fun merge (_ : 'a ord, [] : 'a list, L : 'a list) : 'a list = L
  | merge (_, L, []) = L
  | merge (cmp, x::xs, y::ys) =
      case cmp (x, y) of
        GREATER => y :: merge (cmp, x::xs, ys)
      | _ => x :: merge (cmp, xs, y::ys)

(* msort : 'a ord * 'a list -> 'a list
 * REQUIRES: cmp is an order function
 * ENSURES: msort (cmp, L) ==> L', where L' is a cmp-sorted permutation of L
 *)
fun msort (_ : 'a ord, [] : 'a list) : 'a list = []
  | msort (_, [x]) = [x]
  | msort (cmp, L)  =
      let
        val (A,B) = split L
      in
        merge (cmp, msort (cmp, A), msort (cmp, B))
      end


(* help : 'a ord * 'a list -> 'a list
 * REQUIRES: cmp is an order function and L is sorted
 * ENSURES: help (cmp, L) ==> L', where L' is L with the cmp-duplicates removed
 *)
fun help (cmp : 'a ord, [] : 'a list) : 'a list = []
  | help (cmp : 'a ord, [x] : 'a list) : 'a list = [x]
  | help (cmp : 'a ord, x :: y :: rest : 'a list) : 'a list =
      case cmp (x, y) of
          EQUAL => help (cmp, y :: rest)
        | _ => x :: help (cmp, y :: rest)

(* TEST CASES *)
val cmp = Int.compare

val L = [~3, ~2, 2, 4, 4, 6, 6, 7]
val [~3, ~2, 2, 4, 6, 7] = help (cmp, L)

val L = [2, 2, 4, 4, 7]
val [2, 4, 7] = help (cmp, L)


(* fastDoop : 'a ord * 'a list -> 'a list
 * REQUIRES: cmp is an order function
 * ENSURES: fastDoop (cmp, L) ==> L', where L' is L with the 
 *          cmp-duplicates removed
 *)
fun fastDoop (cmp : 'a ord, [] : 'a list) : 'a list = []
  | fastDoop (cmp : 'a ord, L : 'a list) : 'a list =
    let
      val sortedL = msort (cmp, L)
    in
      help (cmp, sortedL)
    end

(* TEST CASES *)
val cmp = Int.compare

val L = [4, ~2, 6, 4, ~3, 2, 6, 7]
val [~3, ~2, 2, 4, 6, 7] = fastDoop (cmp, L)

val L = [4, 4, 2, 7, 2]
val [2, 4, 7] = fastDoop (cmp, L)