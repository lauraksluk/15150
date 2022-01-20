functor MkEitherMatchable (
  structure A : MATCHABLE
  structure B : MATCHABLE
) :> MATCHABLE =
  struct

    datatype 'a t
      = FromA of 'a A.t
      | FromB of 'a B.t

    exception Invalid of string

    val hide = fn x =>
      FromA (A.hide x)  (* prioritize A over B *)
        handle A.Invalid a_msg => FromB (B.hide x)
          handle B.Invalid b_msg => raise Invalid ("invalid (" ^ a_msg ^ ", " ^ b_msg ^ ")")

    val show = fn _ => raise Fail "Unimplemented"

    val match = fn union => fn f => fn _ => raise Fail "Unimplemented"

    val map = fn f => fn _ => raise Fail "Unimplemented"

  end
