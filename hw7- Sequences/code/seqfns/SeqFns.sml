structure SeqFns :> SEQFNS =
struct
  (* exsisto: ('a -> bool) -> 'a Seq.t -> bool
   * REQUIRES: p is total
   * ENSURES:
   *   if there exists an element x in S such that p x ==> true
   *     then exsisto p S ==> true
   *     else exsisto p S ==> false
   *   exsisto p S has O(|S|) work and O(log|S|) span
   *     if p has O(1) work and span
   *)
  fun exsisto p xs = 
      let
        val S = Seq.filter p xs (* O(|xs|) work *)
                                (* O(log|xs| span *)
      in
        (Seq.length S > 0) (* O(1) work and span *)
      end
  

  (* transposus: 'a Seq.t Seq.t -> 'a Seq.t Seq.t
   * REQUIRES: S is a well-formed n x m matrix
   * ENSURES: transposus S ==> S' where
   *   S' is a well-formed m x n matrix
   *   forall i, j: S[i][j] ~= S'[j][i]
   *  transposus S has O(mn) work
   *  transposus S has O(1) span
   *)
  fun transposus xss =
      let
        val columns = Seq.length (Seq.nth xss 0) (* O(1) work/span *)
        val rows = Seq.length xss (* O(1) work/span *)
      in
        (* O(m) work; O(1) span *)
        Seq.tabulate (fn j => 
            (* O(n) work; O(1) span *)
            Seq.tabulate (fn i => Seq.nth (Seq.nth xss i) j) rows) columns
      end

end

