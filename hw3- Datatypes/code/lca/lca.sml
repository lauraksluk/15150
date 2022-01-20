datatype tree = Empty | Node of tree * int * tree

datatype direction = LEFT | RIGHT

(* find : tree * int -> direction list option
 * REQUIRES: true
 * ENSURES: find (T, v) => NONE if v is not in T, else
 *  SOME L, where L is a list of directions that can be used to traverse
 *  from the root of T to v
 *)
fun find (Empty : tree, v : int) : direction list option = NONE
  | find (Node (L, x, R): tree, v : int) : direction list option =
    if x = v then SOME []
    else let
      val leftS = find (L, v)
      val rightS = find (R, v)
    in
      case leftS of
        SOME y => SOME (LEFT :: y)
      | NONE => case rightS of
                  SOME z => SOME (RIGHT :: z)
                | NONE => NONE
    end


(* TEST CASES *)
val NONE = find (Empty, 0)

val SOME [] = find (Node(Node(Node(Empty,0,Empty),1,Node(
    Empty,2,Empty)),3,Node(Empty,4,Node(Empty,5,Empty))), 3)

val NONE = find (Node(Node(Node(Empty,0,Empty),1,Node(
    Empty,2,Empty)),3,Node(Empty,4,Node(Empty,5,Empty))), 10)

val SOME [RIGHT, RIGHT] = find (Node(Node(Node(Empty,0,Empty),1,Node(
    Empty,2,Empty)),3,Node(Empty,4,Node(Empty,5,Empty))), 5)

val SOME [LEFT, RIGHT] = find (Node(Node(Node(Empty,0,Empty),1,Node(
    Empty,2,Empty)),3,Node(Empty,4,Node(Empty,5,Empty))), 2)

val SOME [RIGHT, LEFT, LEFT, RIGHT] = find (Node(Node(Node(Node(Node(
    Empty,20,Empty),3,Node(Empty,30,Empty)),7,Node(Node(
        Empty,4,Empty),11,Node(Empty,15,Empty))),18,Node(Node(Node(
            Empty,22,Empty),2,Node(Empty,56,Empty)),0,Node(Node(
                Empty,9,Empty),1,Node(Empty,100,Empty)))),5,Node(Node(Node
                (Node(Empty,81,Empty),73,Node(Empty,99,Empty)),16,Node(Node
                (Empty,21,Empty),36,Node(Empty,29,Empty))),28,Node(Node(Node
                (Empty,39,Empty),61,Node(Empty,52,Empty)),19,Node(Node
                (Empty,89,Empty),6,Node(Empty,77,Empty))))), 99)


(* follow : tree * direction list -> tree option
 * REQUIRES: true
 * ENSURES: follow (T, L) => NONE if traversing T according to the directions
 *   in L do not lead to a valid subtree of T, else SOME T', where T' is the
 *   subtree of T that is obtained by traversing T according to L
 *)
fun follow (Empty : tree, [] : direction list) : tree option = SOME Empty
  | follow (Empty : tree, d :: ds : direction list) : tree option = NONE
  | follow (T : tree, [] : direction list) : tree option = SOME T
  | follow (Node (L, x, R) : tree, d :: ds : direction list) = 
        if d = LEFT then follow (L, ds)
        else follow (R, ds)


(* TEST CASES *)
val NONE = follow (Node(Node(Node(Empty,0,Empty),1,Node(Empty,2,Empty)),3,
      Node(Empty,4,Node(Empty,5,Empty))), [RIGHT, LEFT, LEFT])

val SOME (Node(Empty, 2, Empty)) = follow (Node(Node(Node(Empty,0,Empty),1,
      Node(Empty,2,Empty)),3,Node(Empty,4,Node(Empty,5,Empty))), [LEFT, RIGHT])

val SOME (Node(Node(Node(Empty,0,Empty),1,Node(Empty,2,Empty)),3,Node(Empty,4,
      Node(Empty,5,Empty)))) = follow (Node(Node(Node(Empty,0,Empty),1,Node(
          Empty,2,Empty)),3,Node(Empty,4,Node(Empty,5,Empty))), [])

val SOME (Empty) = follow (Node(Node(Node(Empty,0,Empty),1,Node(Empty,2,
      Empty)),3,Node(Empty,4,Node(Empty,5,Empty))), [RIGHT, RIGHT, RIGHT])

val SOME (Node(Empty,4,Node(Empty,5,Empty))) = follow (Node(Node(Node(
    Empty,0,Empty),1,Node(Empty,2,Empty)),3,Node(Empty,4,Node(
      Empty,5,Empty))), [RIGHT])


(* common : direction list * direction list -> direction list
 * REQUIRES: true
 * ENSURES: common (L1, L2) => L, where L contains the longest prefix
 *   that is common to both L1 and L2
 *)
fun common ([] : direction list, L2 : direction list) : direction list = []
  | common (L1 : direction list, [] : direction list) : direction list = []
  | common (x :: xs : direction list, y :: ys : direction list) =
        if x = y then x :: common (xs, ys)
        else []


(* TEST CASES *)
val [] = common ([LEFT, RIGHT, LEFT], [])
val [] = common ([LEFT, LEFT, RIGHT], [RIGHT, LEFT, RIGHT])
val [LEFT, RIGHT] = common ([LEFT, RIGHT, RIGHT], [LEFT, RIGHT, LEFT])
val [LEFT] = common ([LEFT], [LEFT, RIGHT])
val [RIGHT, RIGHT] = common ([RIGHT, RIGHT, LEFT], [RIGHT, RIGHT])


(* lca : tree * int * int -> tree option
 * REQUIRES: T contains no duplicates, a <> b
 * ENSURES: lca (T,a,b) => NONE if a or b is not in T, else
 *   SOME (Node (L,x,R)) such that Node(L,x,R) is a subtree of T and either:
 *             - a is in L and b is in R
 *             - b is in L and a is in R
 *             - a = x and b is in L or R
 *             - b = x and a is in L or R
 *)
fun lca (Empty : tree, a : int, b : int) : tree option = NONE
  | lca (T : tree, a : int, b : int) =
    let
        val a1 = find (T, a)
        val b1 = find (T, b)
    in
        case (a1, b1) of
          (SOME aL, SOME bL) => follow (T, common (aL, bL))
        | _ => NONE
    end


(* TEST CASES *)
val NONE = lca (Node(Node(Node(Empty,0,Empty),1,Node(Empty,2,Empty)),3,
        Node(Empty,4,Node(Empty,5,Empty))), 0, 10)

val SOME (Node(Node(Empty,0,Empty),1,Node(Empty,2,Empty))) = lca (Node(Node
        (Node(Empty,0,Empty),1,Node(Empty,2,Empty)),3,Node(Empty,4,Node(
            Empty,5,Empty))), 0, 2)

val SOME (Node(Node(Empty,0,Empty),1,Node(Empty,2,Empty))) = lca (Node(Node
        (Node(Empty,0,Empty),1,Node(Empty,2,Empty)),3,Node(Empty,4,Node(
            Empty,5,Empty))), 1, 0)

val SOME (Node (Empty, 4, Node (Empty, 5, Empty))) = lca (Node(Node(Node(
        Empty,0,Empty),1,Node(Empty,2,Empty)),3,Node(Empty,4,Node(
            Empty,5,Empty))), 4, 5)

val SOME (Node(Node(Node(Empty,22,Empty),2,Node(Empty,56,Empty)),0,Node(Node(
    Empty,9,Empty),1,Node(Empty,100,Empty)))) = lca (Node(Node(Node(Node(
        Node(Empty,20,Empty),3,Node(Empty,30,Empty)),7,Node(Node(Empty,4,
          Empty),11,Node(Empty,15,Empty))),18,Node(Node(Node(
              Empty,22,Empty),2,Node(Empty,56,Empty)),0,Node(Node(Empty,9,
                Empty),1,Node(Empty,100,Empty)))),5,Node(Node(Node
                (Node(Empty,81,Empty),73,Node(Empty,99,Empty)),16,Node(Node
                (Empty,21,Empty),36,Node(Empty,29,Empty))),28,Node(Node(Node
                (Empty,39,Empty),61,Node(Empty,52,Empty)),19,Node(Node
                (Empty,89,Empty),6,Node(Empty,77,Empty))))), 22, 100)
