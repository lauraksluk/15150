(* Write the Reset structure here! *)
structure Reset :> STUDENT =
struct 
    type thoughts = string
    datatype status
        = YES
        | NO
        | MAYBE of thoughts * Book.direction

    val start = ""
    
    fun think (t: thoughts, (x, p)) = 
        case (x, p) of
          (true, SOME p) => YES
        | _ => MAYBE (t, Book.BACKWARD)

end