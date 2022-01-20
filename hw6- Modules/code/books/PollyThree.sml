(* Write PollyThree, the 150 fanatic, here! *)
structure PollyThree :> STUDENT = 
struct 
    type thoughts = int
    datatype status
        = YES
        | NO
        | MAYBE of thoughts * Book.direction

    val start = 3

    fun isLambda x = (x = "lambda")

    fun think (t, (_, NONE)) : status = NO
      | think (t, (_, SOME p)) =
        let
          val count = List.length(List.filter isLambda p)
        in
          if count >= t then YES
          else MAYBE (t - count, Book.FORWARD)
        end

end
