structure BoolMatchable :> BoolMatchable =
  struct

    datatype ’a t = Empty | Node of 'a t * 'a * 'a t
  
    exception Invalid of "input"
  
    (* turns a ( string * ’a list ) into the custom representation (’a t),
    * or raises Invalid *)
    fun hide ("false" : string, [] : ’a list) = Node(Empty, "false", Empty)
      | hide ("true" : string, [] : 'a list) = Node(Empty, "true", Empty)
      | hide (_, _) = raise Invalid
  
    (* turns our custom representation back into a common format *)
    fun show (Empty: ’a t) = raise Invalid
      | show (Node(Empty, "true", Empty) : 'a t) = ("true", [])
      | show (Node(Empty, "false", Empty) : 'a t) = ("false", [])
   
    
    fun match (op=) (Node(Empty, x, Empty), Node(Empty, y, Empty)) = 

   (’b Subst .t * ’b Subst .t -> ’b Subst .t option )
     -> (’ a * ’b -> ’b Subst .t option )
   -> ’a t * ’b t -> ’b Subst .t option
   

    val map = fn x => x 
  end
