functor MkTest (Book : BOOK) :> TEST =
struct
  exception Error

  val test = fn () =>
    let
      val B0 = Book.bind []

      val () = case Book.currentPage B0 of
        (true, NONE) => ()
      | _ => raise Error

      val () = case Book.unbind B0 of
        [] => ()
      | _ => raise Error

      val B1 = Book.bind [["polly"],["honk"]]

      val () = case Book.currentPage B1 of
        (true, SOME ["polly"]) => ()
      | _ => raise Error

      val B11 = Book.flip Book.FORWARD B1

      val () = case Book.currentPage B11 of
        (false, NONE) => ()
      | _ => raise Error

      val () = case Book.unbind B11 of
        [["polly"],["honk"]] => ()
      | _ => raise Error
      
      val B2 = Book.bind [["1"], ["2"], ["3"], ["4"]]

      val () = case Book.currentPage B2 of
        (true, SOME ["1"]) => ()
      | _ => raise Error

      val B21 = Book.flip Book.FORWARD B2

      val () = case Book.currentPage B21 of
        (false, SOME ["2"]) => ()
      | _ => raise Error

      val B22 = Book.flip Book.FORWARD B21

      val () = case Book.currentPage B22 of
        (false, SOME ["3"]) => ()
      | _ => raise Error

      val B23 = Book.flip Book.BACKWARD B22

      val () = case Book.currentPage B23 of
        (false, SOME ["2"]) => ()
      | _ => raise Error

      val B24 = Book.flip Book.BACKWARD B23

      val () = case Book.currentPage B24 of
        (true, SOME ["1"]) => ()
      | _ => raise Error

      val B3 = Book.flip Book.FORWARD (Book.flip Book.FORWARD B2)

      val () = case Book.currentPage B3 of
        (false, SOME ["3"]) => ()
      | _ => raise Error

    in
      ()
    end
end
