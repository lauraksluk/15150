(* Write a negation functor called MkNemesis here! *)
functor MkNemesis (Student : STUDENT) :> STUDENT =
struct 
    structure Student = Student
    type thoughts = Student.thoughts
    val start = Student.start

    datatype status
        = YES
        | NO
        | MAYBE of thoughts * Book.direction

    fun think (t, (x,y)) : status =
        let
            val old = Student.think (t, (x,y))
        in
            case old of
              Student.YES => NO
            | Student.NO => YES

        end
end