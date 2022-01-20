(* task1 : ('a -> 'b) -> 'a -> 'b *)
fun task1 (f : 'a -> 'b) (x : 'a) = (f x)

(* task2 : ('a -> bool) -> 'a -> ('a -> 'b) -> (unit -> 'b) -> 'b *)
fun task2 (p : 'a -> bool) (x : 'a) (sc : 'a -> 'b) (fc : unit -> 'b) =
    if (p x) then (sc x) else fc ()

(* task3 : ('a -> 'b) -> 'a list -> ('b list -> 'c) -> 'c *)
fun task3 (f : 'a -> 'b) (L : 'a list) (g : 'b list -> 'c) = (g (map f L))
