functor CommMatchable (val name : string) :> MATCHABLE =
  struct

    type 'a t = 'a * 'a

    exception Invalid of string

    val hide = fn
      (s,[x,y]) => (
        if s = name
          then (x,y)
          else raise Invalid "unexpected name"
      )
    | _ => raise Invalid "CommMatchable requires exactly 2 arguments"

    val show = fn (x,y) => (name,[x,y])

    val match = fn union => fn f => fn ((a,b),(x,y)) => raise Fail "Unimplemented"

    val map = fn f => fn (x,y) => (f x,f y)

  end
