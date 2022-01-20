functor AlphaBeta (Settings : SETTINGS) :> PLAYER where Game = Settings.Est.Game =
struct

  structure Est = Settings.Est
  structure Game = Est.Game

  type edge = Game.Move.t * Est.est

  (* Implicit ordering:   NEGINF < Bound v < POSINF for all v *)
  datatype bound = NEGINF | Bound of Est.est | POSINF

  (* invariant: alpha < beta *)
  type alphabeta = bound * bound

  datatype orderAB = BELOW | INTERIOR | ABOVE

  fun estToString (Est.Definitely oc) = "Definitely(" ^ Game.Outcome.toString oc ^ ")"
    | estToString (Est.Guess g) = "Guess(" ^ Est.toString g ^ ")"

  fun valueToString v = "Value(" ^ estToString v ^ ")"

  fun edgeToString (m, v) =
    "Edge(" ^ Game.Move.toString m ^ ", " ^ estToString v ^ ")"

  fun boundToString NEGINF = "NEGINF"
    | boundToString POSINF = "POSINF"
    | boundToString (Bound v) = "Bound(" ^ estToString v ^ ")"

 fun abToString (a,b) =
   "(" ^ boundToString a ^ "," ^ boundToString b ^ ")"

  (* lesseq: Est.est * Est.est -> bool
   * REQUIRES: true
   * ENSURES: lesseq (x, y) ==> true if x <= y according to Est.compare
   *          and false otherwise
   *)
  fun lesseq (x,y) =
    case Est.compare (x,y) of
      GREATER        => false
    | (LESS | EQUAL) => true

  (* compareBoundToEst : bound * Est.est -> order
   * REQUIRES: true
   * ENSURES: compareBoundToEst (b, v) ==>
   *                              LESS     if b < v
   *                              EQUAL    if b = v
   *                              GREATER  if b > v
   *          where NEGINF < Bound v < POSINF and if
   *          b = Bound x then x and v are compared
   *          according Est.compare
   *)
  fun compareBoundToEst (NEGINF ,_) = LESS
    | compareBoundToEst (POSINF ,_) = GREATER
    | compareBoundToEst (Bound x,y) = Est.compare (x,y)

  (* compareAB : alphabeta -> Est.est -> orderAB   *)
  (* compareAB (a,b) v  ==>                        *)
  (*                 BELOW      if  v <= a         *)
  (*                 INTERIOR   if  a < v < b      *)
  (*                 ABOVE      if  b <= v         *)
  fun compareAB (a,b) (v : Est.est) =
    case (compareBoundToEst (a,v), compareBoundToEst (b,v)) of
      (GREATER, GREATER) => BELOW
    | (EQUAL  , GREATER) => BELOW
    | (LESS   , GREATER) => INTERIOR
    | (LESS   , LESS   ) => ABOVE
    | (LESS   , EQUAL  ) => ABOVE
    | _ => raise Fail "Invalid a-b Bound"


  (* maxEdge : edge option -> edge -> edge option                      *)
  (* REQUIRES: true                                                    *)
  (* ENSURES:  maxEdge e1op e2 returns SOME(the edge with max value),  *)
  (*           where the max value is defined as e2 if e1op = NONE,    *)
  (*           e1 if v1 >= v2 according to Est.compare, and e2         *)
  (*           otherwise.                                              *)
  fun maxEdge NONE e = SOME(e)
    | maxEdge (SOME(m1,v1)) (m2,v2) = SOME(if lesseq(v2,v1) then (m1,v1) else (m2,v2))

  (* minEdge : edge option -> edge -> edge option                      *)
  (* REQUIRES: true                                                    *)
  (* ENSURES:  minEdge e1op e2 returns SOME(the edge with min value),  *)
  (*           where the min value is defined as e2 if e1op = NONE,    *)
  (*           e1 if v1 <= v2 according to Est.compare, and e2         *)
  (*           otherwise.                                              *)
  fun minEdge NONE e = SOME(e)
    | minEdge (SOME(m1,v1)) (m2,v2) = SOME(if lesseq(v1,v2) then (m1,v1) else (m2,v2))


  (* search : int -> alphabeta -> Game.State.t -> edge option          *)
  (* REQUIRES: d > 0, (Game.moves s) is nonempty.                      *)
  (* ENSURES:  search d ab s ==> SOME e, the optimal outgoing edge     *)
  (*           from s, based on depth-d alpha-beta prunings,           *)
  (*           starting from alpha-beta interval "ab". NONE is only    *)
  (*           returned when the input does not satisfy the invariants *)
  (*                                                                   *)
  (* search uses helper functions maxisearch and minisearch to perform *)
  (* the actual search, including updating the alpha-beta interval     *)
  (* and the best edge seen so far, as well as any possible pruning.   *)
  fun search (d : int) (ab : alphabeta) (s : Game.State.t) : edge option =
      case (Game.player s) of
        Player.Minnie => minisearch d ab s (Game.moves s) NONE
      | Player.Maxie => maxisearch d ab s (Game.moves s) NONE


  (* maxisearch : int -> alphabeta -> Game.State.t ->                  *)
  (*              Game.Move.t Seq.seq -> edge option -> edge option    *)
  (* REQUIRES: d > 0;                                                  *)
  (*           "moves" should contain only moves that are legal at s;  *)
  (*           "s" is a Maxie state;                                   *)
  (*           "best" should not be NONE when "moves" is empty.        *)
  (* ENSURES:  maxisearch d ab s moves best ==>                        *)
  (*           SOME e, an optimal outgoing edge from s, based on       *)
  (*           depth-d alpha-beta pruning over "moves",starting        *)
  (*           from alpha-beta interval "ab", with accumulator         *)
  (*           "best" as default if no better edge is found.           *)
  and maxisearch d (ab as (a,b)) s moves best = 
      case (Seq.showl moves) of
        Seq.Nil => best
      | Seq.Cons (m, ms) => 
            (let
              val makeMove = Game.play (s, m)
              val value = evaluate (d - 1) ab makeMove
              val currBest = maxEdge best (m, value)
            in
              case (compareAB ab value) of 
                BELOW => maxisearch d ab s ms currBest
              | INTERIOR => maxisearch d (Bound value, b) s ms currBest
              | ABOVE => currBest
            end)


  (* minisearch : int -> alphabeta -> Game.State.t ->                  *)
  (*              Game.Move.t Seq.seq -> edge option -> edge option    *)
  (* REQUIRES: d > 0;                                                  *)
  (*          "moves" should contain only moves that are legal at s;   *)
  (*           "s" is a Minnie state;                                  *)
  (*           "best" should not be NONE when "moves" is empty.        *)
  (* ENSURES:  minisearch d ab s moves best ==> SOME e, an optimal     *)
  (*           outgoing edge from s, based on depth-d alpha-beta       *)
  (*           pruning over "moves", starting from alpha-beta interval *)
  (*           "ab", with accumulator "best" as default if no better   *)
  (*           edge is found.                                          *)
  and minisearch d (ab as (a,b)) s moves best = 
      case (Seq.showl moves) of
        Seq.Nil => best
      | Seq.Cons (m, ms) => 
            (let
              val makeMove = Game.play (s, m)
              val value = evaluate (d - 1) ab makeMove
              val currBest = minEdge best (m, value)
            in
              case (compareAB ab value) of 
                BELOW => currBest
              | INTERIOR => minisearch d (a, Bound value) s ms currBest
              | ABOVE => minisearch d ab s ms currBest
            end)


  (* evaluate : int -> alphabeta -> Game.status -> Est.est             *)
  (* REQUIRES: d >= 0                                                  *)
  (* ENSURES:  evaluate d ab s ==> v, the value attributed to status   *)
  (*           st, based on depth-d alpha-beta search.                 *)
  and evaluate d (ab as (a,b)) st = 
      let
        fun searchForSolution state = 
            case (search d ab state) of
              SOME (move, value) => value
            | NONE => raise Fail ""
      in
        case st of
          Game.Done outcome => Est.Definitely outcome
        | Game.Playing state => 
            if (d = 0 orelse (Seq.null (Game.moves state)))
            then Est.Guess (Est.estimate state)
            else searchForSolution state
      end
      


  (* next_move : Game.State.t -> Game.Move.t                           *)
  (* REQUIRES: s is a valid game state                                 *)
  (* ENSURES: next_move s ==> m, where m is the desired move to make.  *)
  fun next_move s = 
      let
        val d = Settings.search_depth
        val (a, b) = (NEGINF, POSINF)
      in
        case (search d (a,b) s) of
          SOME (move, value) => move
        | NONE => raise Fail ""
      end

end
