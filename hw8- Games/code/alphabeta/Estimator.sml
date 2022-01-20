structure Estimator :> ESTIMATOR where Game = Checkers =
struct

  structure Game = Checkers

  datatype player = datatype Player.t  (* datatype replication *)

  type guess = int
  datatype est = Definitely of Game.Outcome.t | Guess of guess

  val compare = fn
    (Definitely (SOME p1)    ,Definitely (SOME p2)    ) => Player.compare (p1,p2)
  | (Definitely NONE         ,Definitely NONE         ) => EQUAL
  | (Definitely (SOME Minnie),_                       ) => LESS
  | (Definitely (SOME Maxie) ,_                       ) => GREATER
  | (Definitely NONE         ,Guess n                 ) => Int.compare (0,n)
  | (_                       ,Definitely (SOME Minnie)) => GREATER
  | (_                       ,Definitely (SOME Maxie) ) => LESS
  | (Guess n                 ,Definitely NONE         ) => Int.compare (n,0)
  | (Guess a                 ,Guess b                 ) => Int.compare (a,b)

  val toString = Int.toString

  (* Examining current board, to determine which player has more pieces belonging to
   * them on the board. When counting pieces, pieces that are "kinged" are worth twice 
   * as more since they can move in either direction. 
   * The player with more pieces belonging to them and/or have more pieces "kinged"
   * is more likely to win. *)
  fun estimate (board : Checkers.State.square Mat.t, player : Player.t, _) : guess =
      let
        fun currentBoard (row, col) =
            (case (Mat.nth board (row, col)) of 
              NONE => 0
            | SOME (Maxie, true) => 2
            | SOME (Maxie, false) => 1
            | SOME (Minnie, true) => ~2
            | SOME (Minnie, false) => ~1)
        
        val (numRows, numCols) = Mat.size board
        val scores = Mat.tabulate currentBoard (numRows, numCols)
      in
        Mat.reduce (op+) 0 scores
      end

end
