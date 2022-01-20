functor MkPotionMix (Collect: COLLECT) :> POTIONMIX =
struct

  (* you may assume this is a refsol for collect *)
  val collect = Collect.collect

  (* you may not assume there are only 4 colors *)
  type vertex = int
  type edge = vertex * vertex * Color.t

  (* A graph is represented as a "3-dimensional" sequence of edges.
   * For each edge, E = (v1, v2, color),
   *  - each subsequence describes the edges stemming from vertex v1.
   *  - each inner subsequence within are the edges having the same v1, v2 *)
  type graph = edge Seq.t Seq.t Seq.t
  
  (* mkGraph: edge Seq.t -> graph
   * REQUIRES:
   *    For every edge in E, E also contains the opposite edge of the same color
   *    E contains no duplicates
   *    Every vertex is adjacent to some edge
   *    There are no self-loops
   * ENSURES: mkGraph E ==> G where
   *    mkGraph E has O(|E|log|E|) work
   *    mkGraph E has O(log^2 |E|) span
   *)
  fun mkGraph E =
      let
        fun group1 ((v1, _, _), (v2, _, _)) = Int.compare (v1, v2) (* O(1) work/span *)
        fun group2 ((_, v1, _), (_, v2, _)) = Int.compare (v1, v2) (* O(1) work/span *)

        val collectE = collect group1 E                (* O(|E|log|E|) work *)                                         
                                                       (* O(log^2|E|) span *)

        val graph = Seq.map (collect group2) collectE  (* O(|E|log|E|) work *)                                         
                                                       (* O(log^2|E|) span *)
      in
        graph
      end

  (* filterNeighbors: (Color.t -> bool) -> vertex -> graph -> vertex Seq.t
   * REQUIRES:
   *    p is total
   *    p has O(1) work/span
   *    G is a valid graph
   *    v is a vertex in G
   * ENSURES: filterNeighbors p v G ==> N where
   *    x is int N if there exists a color c such that p c ~= true
   *      and (v,x,c) is an edge in G
   *    N is sorted and contains no duplicates
   *    filterNeighbors p v G has O(d) work if p has of O(1) work
   *      and v has d edges incident to it in G
   *    filterNeighbors p v G has O(log km) where k is the number of possible
   *      colors and v has m neighbors
   *)
  fun filterNeighbors p v G =
      let
        val vNeighbors = Seq.nth G v (* O(1) work/span *)
        fun notEmpty s = not (Seq.null s) (* O(1) work/span *)
        fun f (_, _, color) = (p color) (* O(1) work/span *)

        val filtered = Seq.map (Seq.filter f) vNeighbors (* O(d) work *)
                                                         (* O(log d) span *)
        val filteredN = Seq.filter (notEmpty) filtered   (* O(d) work *)
                                                         (* O(log d) span *)

        val len = Seq.length filteredN (* O(1) work/span *)
        fun getVert i = 
          (let
            val (_, v, _) = (Seq.nth (Seq.nth filteredN i) 0) (* O(1) work/span *)
          in
            v
          end)

      in
        Seq.tabulate getVert len (* O(d) work *)
                                 (* O(1) span *)
      end


  (* neighbors: vertex -> graph -> vertex Seq.t
   * REQUIRES:
   *    G is a valid graph
   *    v is a vertex in G
   * ENSURES: neighbors v G ==> N where
   *    x in N iff there is a c: color such that (v, x, c) is an edge in G
   *    N is sorted and contains no duplicates
   *    If v has m neighbors, neighbors v G has O(m) work
   *    neighbors v G has O(1) span
   *)
  fun neighbors v G =
      let 
        val edges = Seq.nth G v (* O(1) work/span *)
        val len = Seq.length edges (* O(1) work/span *)
        fun getVert i = 
          (let
            val (_, v, _) = (Seq.nth (Seq.nth edges i) 0) (* O(1) work/span *)
          in 
            v
          end)

      in
        Seq.tabulate getVert len (* O(m) work *)
                                 (* O(1) span *)
      end



  (* mixPotion: graph -> vertex Seq.t
   * REQURES: G is a valid graph with at least one blue edge
   * ENSURES: mixPotion G ==> P where
   *    P is sorted and contains no duplicates
   *    v is the lowest-numbered vertex in G with the most incident blue edges
   *    If G has n vertices and the maximum number of edges incident to a
   *    vertex in G is d, mixPotion G has O(nd) work
   *    If there are k different possible colors and a vertex in G has at
   *      most m neighbors, mixPotion G has O(log(kmn)) span
   *)
  fun mixPotion G =
      let
        fun isBlue c = Color.eq (c, Color.blue) (* O(1) work/span *)
        fun numBlue v G = Seq.length (filterNeighbors isBlue v G) (* O(d) work *)
                                                                  (* O(log km) span *)
        val len = Seq.length G   (* O(1) work/span *)
        
        fun countBlues x = (x, (numBlue x G))         (* O(d) work *)
                                                      (* O(log km) span *)
        
        val blues = Seq.tabulate countBlues len       (* O(nd) work *)
                                                      (* O(d) span *)
        (* O(1) work/span *)
        fun findMax ((v1, b1), (v2, b2)) =            
            if b2 > b1 then (v2, b2) else (v1, b1)    

        val (b, _) = Seq.reduce findMax (0, 0) blues  (* O(nd) work *)
                                                      (* O(1) span *)
        val bNeighbors = filterNeighbors isBlue b G   (* O(d) work *)
                                                      (* O(log km) span *)
        val maxBlue = Seq.singleton b (* O(1) work/span *)
      in
        Seq.merge Int.compare (bNeighbors, maxBlue) (* O(m) work *)
                                                    (* O(log m) span *)
      end
end
