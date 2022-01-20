structure Solver :> PLAYER where Game = Mines =
struct

  structure Game = Mines

  structure Var : EQ =
  struct
    type t = Mat.index
    fun equal ((i1, j1), (i2, j2)) =
      i1 = i2 andalso j1 = j2
  end

  structure SAT = MkSATSolver (
    Var : EQ
  )

  structure Set = SAT.Set

  fun solve (b : Mines.State.board) : (Mat.index * bool) list = 
      let
        fun tileStatus ((i, j), state) =
            let
              val mine = (Set.singleton (i, j), 1)
              val safe = (Set.singleton (i, j), 0)
              val adjacent = Mat.neighborsIdx b (i, j)
            in
              case state of
                NONE => Seq.empty()
              | SOME (Mines.State.Flag) => Seq.singleton (mine)
              | SOME (Mines.State.Safe w) => Seq.fromList [safe, 
                                                  (Set.fromSeq (adjacent), w)]
            end
        
        fun findIntersect ((f1, t1), (f2, t2)) = 
            let
              val half1 = Set.intersection (f1, f2)
              val half2 = Set.intersection (t1, t2)
            in
              (half1, half2)
            end

        exception EmptySolution

        val possibleSol = SAT.solve (Mat.reduce Seq.append (Seq.empty()) 
                                                        (Mat.mapIdx tileStatus b))
        val (F, T) = if (Seq.null possibleSol) then raise EmptySolution
                     else Seq.reduce1 findIntersect possibleSol
        
        fun substFalse x = (x, false)
        fun substTrue x = (x, true)
        
        fun removeNones ((i, j), _) = case Mat.nth b (i, j) of
                                        NONE => true
                                      | SOME _ => false

        val sols = Seq.append (Seq.map substFalse (Set.toSeq F),
                               Seq.map substTrue (Set.toSeq T))
      in
        Seq.toList (Seq.filter removeNones sols)
        handle EmptySolution => []
      end

  fun next_move (b, n, h) = 
      case solve b of
        [] => Mines.Move.Choose NONE
      | (m :: ms) => Mines.Move.Choose (SOME m)


end
