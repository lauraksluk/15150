functor MkCompose (StudentA : STUDENT
                   StudentB : STUDENT) =
struct
    type thoughts = 'a
    datatype status
        = YES
        | NO
        | MAYBE of thoughts * Book.direction

    structure StudentA = Student
    structure StudentB = Student

    val start = StudentA.start
    val B = StudentB.start

    fun bThink (B, (x, SOME p)) =
        case StudentB.think(B, (x, SOME p)) of
          StudentB.NO => NO
        | StudentB.MAYBE (b, _) => MAYBE (b, Book.FORWARD)
        | StudentB.YES => YES
    
    fun think (t, (x, NONE)) = NO
      | think (t, (x, SOME p)) =
        let
          val sA = StudentA.think(start, (x, SOME p))
        in
          case sA of
            StudentA.NO => NO
          | StudentA.MAYBE (a, _) => MAYBE (a, Book.FORWARD)
          | StudentA.YES => bThink(B, (x, SOME p))
        end
end 