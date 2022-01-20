(* eval : environ * exp -> int
 * REQUIRES: All variables in e have one entry in env.
 * ENSURES: eval (env,e) ==> n, where n is the integer representing
 * the value of e given environment env.
 *)
fun eval (env : environ, e : exp) : int = 
    case e of
        Var x => lookup(env, x)
      | Int x => x
      | Add (a, b) => eval (env, a) + eval (env, b)
      | Mul (a, b) => eval (env, a) * eval (env, b)
      | Not a => if eval (env, a) = 0 then 1 else 0
      | IfThenElse (i, t, e) => if eval (env, i) = 0 
                                then eval (env, e) else eval (env, t)

(* TEST CASES *)
val 0 = eval([("x", 0)], Add(Var "x", Int 0))
val 150 = eval([("x", 45), ("y", 3)], Add(Int 15, Mul(Var "x", Var "y")))