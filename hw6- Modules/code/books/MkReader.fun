(* Your MkReader functor, taking in a Student structure, goes here. *)
functor MkReader (Student : STUDENT) :> READER =
struct 
    structure Student = Student

    fun like (b: Book.book, t : Student.thoughts) : Student.status =
        let
            val pg = Book.currentPage b 
        in 
            (case Student.think(t, pg) of
              Student.YES => Student.YES
            | Student.NO => Student.NO
            | Student.MAYBE (thought, dir) => like ((Book.flip dir b), thought))
        end

    fun read (b : Book.book) : bool = 
        let
            val t = Student.start
        in
            case like (b, t) of 
              Student.YES => true
            | Student.NO => false
        end

end

