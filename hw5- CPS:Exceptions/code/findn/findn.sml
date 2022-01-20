datatype 'a shrub = Leaf of 'a
                  | Branch of 'a shrub * 'a shrub

(* findOne : ('a -> bool) -> 'a shrub -> ('a -> 'b) -> (unit -> 'b) -> 'b
 * REQUIRES: p is total
 * ENSURES:  findOne p T sc fc ==> s x if x is in T and p(x) ==> true.
 *           If no such x exists, this evalutes to fc () instead.
 *           If more than one such x exists,
 *           findOne p T sc fc evaluates the leftmost such x.
 *)
fun findOne (p : 'a -> bool) (Leaf x : 'a shrub)
            (sc : 'a -> 'b) (fc : unit -> 'b) : 'b = if p x then sc x else fc()
  | findOne p (Branch(L, R)) sc fc = 
            findOne p L sc (fn () => findOne p R sc fc)
             
(* TEST CASES *)
val T = Branch(Leaf 1, Leaf 2)
val p = fn x => x > 0
val sc = SOME
val fc = fn () => NONE
val SOME 1 = findOne p T sc fc 

val T = Branch(Branch(Leaf 1, Leaf 2), Branch(Leaf 3, Leaf 4))
val p = fn x => x > 3
val sc = SOME
val fc = fn () => NONE
val SOME 4 = findOne p T sc fc


(* findTwo : ('a -> bool) -> ('a * 'a -> bool) -> 'a shrub ->
 *           ('a * 'a -> 'b) -> (unit -> 'b) -> 'b
 * REQUIRES: p is total, eq is total, eq represents an equivalence relation
 * ENSURES:  findTwo p eq T sc fc ==> sc(x,y) if x and y are values in T s.t.
 *           p(x) ==> true and p(y) ==> true and eq(x,y) ==> false.
 *           If no such pair (x,y) exists, findTwo p T sc fc ==> fc ()
 *)
fun findTwo (p : 'a -> bool) (eq : 'a * 'a -> bool) (T : 'a shrub)
            (sc : 'a * 'a -> 'b) (fc : unit -> 'b) : 'b  = 
        findOne p T (fn a => 
            findOne (fn b => (p b) andalso not (eq (a, b))) T 
                    (fn c => sc(a, c)) fc) fc

(* TEST CASES *)
val T1 = Branch(Leaf 1, Leaf 2)
val T2 = Branch(Leaf 1, Leaf 1)
val p1 = fn x => x > 0
val p2 = fn x => x = 1
val eq = op=
val sc = SOME
val fc = fn () => NONE

val SOME (1, 2) = findTwo p1 eq T1 sc fc
val NONE = findTwo p2 eq T1 sc fc
val NONE = findTwo p1 eq T2 sc fc



(* findN : ('a -> bool) -> ('a * 'a -> bool) -> 'a shrub -> int ->
 *         ('a list -> 'b) -> (unit -> 'b) -> 'b
 * REQUIRES: n >= 0, p is total, eq is total, eq represents an equivalence relation
 * ENSURES: findN p eq T n sc fc ==> sc L if there exists a list L of length n s.t
 *          the elements in L are pairwise distinct by eq, and for each element x in L,
 *          p x ==> true.
 *          Otherwise evaluates to fc ().
 *)
fun findN (p : 'a -> bool) (eq : 'a * 'a -> bool) (T : 'a shrub)
          (n : int) (sc : 'a list -> 'b) (fc : unit -> 'b) =
  case n of
    0 => sc []
  | _ =>
      let
        fun success x = findN (fn y => (p y) andalso not (eq(x, y))) eq T 
                              (n - 1) (fn z => sc(x :: z)) fc
      in
        findOne p T success fc
      end

(* TEST CASES *)
val T = Branch(Branch(Leaf 1, Leaf 2), Branch(Leaf 3, Leaf 4))
val p1 = fn x => x > 0
val p2 = fn x => x > 4
val p3 = fn x => x mod 2 = 0
val eq = op=
val sc = SOME
val fc = fn () => NONE

val SOME [1,2,3,4] = findN p1 eq T 4 sc fc 
val NONE = findN p2 eq T 4 sc fc
val SOME [2,4] = findN p3 eq T 2 sc fc

