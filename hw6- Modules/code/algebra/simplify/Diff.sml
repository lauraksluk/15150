structure Diff :> RULES =
  struct

    val const = fn name => STerm.hide (name,[])

    val theta = const "theta"

    val zero = const "zero"
    val succ = fn x => STerm.hide ("succ",[x])
    val op + = fn (x,y) => STerm.hide ("+",[x,y])
    val op * = fn (x,y) => STerm.hide ("*",[x,y])

    val sin = fn x => STerm.hide ("sin",[x])
    val cos = fn x => STerm.hide ("cos",[x])

    val diff = fn (x,y) => STerm.hide ("diff",[x,y])

    val x = STerm.Variable "x"
    val y = STerm.Variable "y"
    val t = STerm.Variable "t"

    infixr ==>
    val (op ==>) = fn (x,y) => (x,y)  (* syntactic sugar *)

    val rules : Simp.rule list = [
      (* TODO *)
    ]

  end
