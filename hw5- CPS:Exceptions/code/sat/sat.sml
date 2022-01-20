datatype prop = Const of bool
              | Var of string
              | And of prop * prop
              | Or of prop * prop
              | Not of prop

type assignment = string * bool


(* subst : assignment * prop -> prop
 * REQUIRES: true
 * ENSURES: subst ((c, b), p) ==>* p' such that
 *  - p' does not contain c
 *  - p' is equivalent to p except all instances of c have been replaced with
 *    True if b is true and False if b is false.
 *)
fun subst ((c, b), Var c') = if c = c' then Const b else Var c'
  | subst ((c, b), And (p1, p2)) = And (subst ((c, b), p1), subst ((c, b), p2))
  | subst ((c, b), Or (p1, p2)) = Or (subst ((c, b), p1), subst ((c, b), p2))
  | subst ((c, b), Not p) = Not (subst ((c, b), p))
  | subst (_, p) = p

(* subst_all : prop -> assignment list -> prop
 * REQUIRES: all strings in assignments are unique
 * ENSURES: subst_all p assignments evaluates to a proposition with all
 *  instances of variables in assignments replaced with the True or False,
 *  according to their corresponding booleans in assignments.
 *)
val subst_all = foldl subst

(* try_eval : prop -> (bool -> 'a) -> (string -> 'a) -> 'a
 * REQUIRES: true
 * ENSURES: if p contains no variables,
 *  then try_eval p s k ==>* s b where b is the truth value of p.
 *  Otherwise, try_eval p s k ==>* k c where c is the leftmost variable in p.
 *)
fun try_eval (p : prop) (sc : bool -> 'a) (fc : string -> 'a) : 'a =
      case p of
        Const b => sc b
      | Var s => fc s 
      | And (p1, p2) => try_eval p1 (fn x => try_eval p2 
                                    (fn y => sc (x andalso y)) fc) fc
      | Or (p1, p2) => try_eval p1 (fn x => try_eval p2
                                   (fn y => sc (x orelse y)) fc) fc
      | Not p1 => try_eval p1 (fn x => sc (not x)) fc 

(* TEST CASES *)
val (SOME false, NONE) = try_eval (And (Not (Const true), Const true))
                                  (fn b => (SOME b, NONE))
                                  (fn c => (NONE, SOME c))



(* sat : prop -> (assignment list -> 'a) -> (unit -> 'a) -> 'a
 * REQUIRES: true
 * ENSURES: if there exists some value asgn such that
 *  try_eval (subst_all assignments) (fn x => x) (fn () => false) ==>* true
 *  then sat p sc fc ==>* sc asgn.
 *  Otherwise, sat p sc fc ==>* fc ().
 *)
fun sat (p : prop) (sc : assignment list -> 'a) (fc : unit -> 'a) : 'a =
      try_eval p (fn x => if x then sc [] else fc())
      (fn x => sat (subst ((x, true), p)) (fn y => sc ((x, true) :: y))
      (fn () => sat (subst ((x, false), p)) (fn y => sc ((x, false) :: y)) fc))

(* TEST CASES *)
val NONE = sat (And (Not (Var "a"), Var "a")) SOME (fn () => NONE)
val SOME [("a", true)] = sat (Or (And (Not (Var "a"), Var "a"), Var "a")) SOME
                             (fn () => NONE)


