structure Collect :> COLLECT =
struct
  (* collect: ('a * 'a -> order) -> 'a Seq.t -> 'a Seq.t Seq.t
   * REQUIRES: cmp is "well-behaved"
   *    cmp is total
   *    cmp(a,b) has O(1) work/span
   * ENSURES: collect cmp S ==> C where
   *    Every element in S appears in some element of C
   *      (Note that elements of C are sequences)
   *    If you sum the lengths of the sequences in C, you get the length of S
   *    Elements in each sequence in C are cmp-EQUAL
   *    Elements in earlier sequences in C are cmp-LESS than elements in
   *      later sequences in C
   *    No element in C is empty
   *      (elements in C are sequences)
   *    collect cmp S has O(|S|log|S|) work
   *    collect cmp S has O(log^2|S|) span
   *)
  fun collect cmp xs =
    if Seq.length xs = 0 then Seq.empty() (* O(1) work/span *)
    else if Seq.length xs = 1 then Seq.singleton xs (* O(1) work/span *)
    else
      let
        val sortedXS = Seq.sort cmp xs (* O(|S|log|S|) work *)
                                       (* O(log^2|S|) span *)

        val sLen = Seq.length sortedXS (* O(1) work/span *)

        (* O(|S|) work; O(1) span *)
        val shift = Seq.append (Seq.singleton (Seq.nth sortedXS 0), sortedXS)
        val tuples = Seq.zip (shift, sortedXS)
        val enum = Seq.enum (Seq.map cmp tuples)

        (* O(|S|) work; O(log|S|) span *)
        val divisions = Seq.filter (fn (_, ord) => ord = LESS) enum

        (* O(1) work/span *)
        fun nthSplit i =
            let
              val (index, _) = Seq.nth divisions i  (* O(1) work/span *)
            in
              index
            end
        val len = Seq.length divisions  (* O(1) work/span *)
        fun split 0 = Seq.subseq sortedXS (0, (nthSplit 0))  (* O(1) work/span *)
          | split x = 
              if x < len 
              then Seq.subseq sortedXS                   (* O(1) work/span *)
                          (nthSplit(x - 1), ((nthSplit x) - nthSplit(x - 1)))
              else Seq.subseq sortedXS                   (* O(1) work/span *)
                          (nthSplit(x - 1), (sLen - nthSplit(x - 1)))
      in
        Seq.tabulate split (len + 1) (* O(|S|) work *)
                                     (* O(1) span *)
      end
    
end
