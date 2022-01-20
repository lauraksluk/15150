signature FOREST =
  sig
    structure T : TREE
    type 'a forest
    exception Full
    exception NotInForest
    val limit : int
    val empty : unit -> 'a forest
    val addToForest : 'a forest -> 'a T.tree -> 'a forest
    val addToTree : ('a * 'a -> order) -> 'a forest -> 'a -> 'a -> 'a forest
    val findRoot : ('a * 'a -> order) -> 'a forest -> 'a -> 'a
  end
