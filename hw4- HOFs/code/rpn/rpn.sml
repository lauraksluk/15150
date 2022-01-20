exception InvalidToken
exception InvalidExpression

datatype token =
    Integer of int
  | Multiply
  | Add
  | Subtract
  | Divide
  | SumAll

fun tokenize "*" = Multiply
  | tokenize "+" = Add
  | tokenize "-" = Subtract
  | tokenize "/" = Divide
  | tokenize "sum" = SumAll
  | tokenize numberString =
      case Int.fromString numberString of
        SOME x => Integer x
      | NONE => raise InvalidToken

(* We can't write the usual `raise Fail "Unimplemented!"` because these are val
 * bindings, not fun bindings (the exception would be raised right away).
 * Instead, we use `const : 'a -> 'b -> 'a` to make our stubs *)
fun const a b = a

(* parse : string -> token list *)
val parse : string -> token list 
    = map(tokenize) o (String.tokens(fn #" " => true | _ => false))

(* TEST CASES *)
val [Integer 3, Integer 2, Add] = parse "3 2 +"
val [Integer 3, Integer 1, Integer 5, Add, Multiply, Integer 15, Subtract]
                                = parse "3 1 5 + * 15 -"


(* addToStack : token * int list -> int list
 * REQUIRES: true
 * ENSURES: addToStack(x, L) => L' where L' is the current working
 *          stack during processing of token x
 *)
fun addToStack (x : token, a :: b :: rest : int list) : int list =
    (case x of
      Integer n => n :: (a :: b :: rest)
    | SumAll => [foldl (op +) 0 (a :: b :: rest)]
    | Add => (b + a) :: rest
    | Multiply => (b * a) :: rest
    | Subtract => (b - a) :: rest
    | Divide => (b div a) :: rest)
  
  | addToStack (x : token, L : int list) : int list = 
    (case x of
        Integer n => n :: L
      | SumAll => [foldl (op +) 0 L]
      | _ => raise InvalidExpression)

(* TEST CASES *)
val [3] = addToStack (Integer 3, [])
val [7] = addToStack (Add, [3, 4])

(* eval : token list -> int list *)
val eval : token list -> int list = foldl addToStack []

(* TEST CASES *)
val [] = eval ([])
val [0] = eval([SumAll])
val [3] = eval([Integer 3, Integer 1, Integer 5, Add, Multiply, Integer 15, 
                Subtract])


(* rpn : string -> int list *)
val rpn : string -> int list = eval o parse

(* TEST CASES *)
val [] = rpn ""
val [0] = rpn "sum"
val [3] = rpn "3 1 5 + * 15 -"
