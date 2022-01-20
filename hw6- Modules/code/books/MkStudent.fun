(* Your functor MkStudent goes here. *)
functor MkStudent (val count : int 
                   val favoriteWord : string) :> STUDENT =
struct
    type thoughts = int
    datatype status
        = YES
        | NO
        | MAYBE of thoughts * Book.direction

    val start = count

    fun isWord x = (x = favoriteWord)

    fun think (t, (_, NONE)) : status = NO
      | think (t, (_, SOME p)) =
        let
          val currCount = List.length(List.filter isWord p)
        in
          if currCount >= t then YES
          else MAYBE (t - currCount, Book.FORWARD)
        end

end