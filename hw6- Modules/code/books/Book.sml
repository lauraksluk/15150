(* Implement your Book structure here! *)
structure Book :> BOOK =
struct
    type page = string list
    datatype direction = FORWARD | BACKWARD
  
    type book = page * page list * page list
    exception OutOfBounds

    fun bind ([] : page list) : book = ([], [], [])
      | bind (x :: xs : page list) : book = (x, [], xs)
    
    fun unbind (([], [], []) : book) = []
      | unbind ((p, prev, next) : book) : page list = 
            List.rev(prev) @ (p :: next)

    fun currentPage (([], [], []) : book) : bool * page option = (true, NONE)
      | currentPage ((p, [], next) : book) = (true, SOME p)
      | currentPage ((p, prev, []) : book) = (false, NONE)
      | currentPage ((p, prev, next) : book) = (false, SOME p)

    fun flip (FORWARD : direction) (p, prev, []) : book = raise OutOfBounds
      | flip (FORWARD : direction) (p, prev, x :: xs) = (x, p :: prev, xs) 
      | flip (BACKWARD : direction) (p, [], next) = raise OutOfBounds
      | flip (BACKWARD : direction) (p, y :: ys, next) = (y, ys, p :: next)

end
    
