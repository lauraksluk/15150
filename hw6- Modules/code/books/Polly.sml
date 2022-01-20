(* Make the 150 TAs happy by implementing their parrot, Polly! *)
structure Polly :> STUDENT = 
struct 
    type thoughts = string
    datatype status
        = YES
        | NO
        | MAYBE of thoughts * Book.direction

    val start = "lambda"

    fun isLambda x = (x = start)

    fun think (t, (_, NONE)) : status = NO
      | think (t, (_, SOME p)) =
        if (List.exists isLambda p) then YES
        else MAYBE (t, Book.FORWARD)
        
end
