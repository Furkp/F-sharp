module AExpSimple

type aExp =
    | N of int
    | Add of aExp * aExp
    | Sub of aExp * aExp
    | Mul of aExp * aExp

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

// 3.1

let rec arithEvalSimple = 
    function
    | N(x) -> x
    | Add(x, y) -> arithEvalSimple(x) + arithEvalSimple(y)
    | Sub(x, y) -> arithEvalSimple(x) - arithEvalSimple(y)
    | Mul(x, y) -> arithEvalSimple(x) * arithEvalSimple(y)