module AExpState

type aExp =
    | N of int
    | V of string
    | Add of aExp * aExp
    | Sub of aExp * aExp
    | Mul of aExp * aExp

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)


type state = Map<string, int>

let rec arithEvalState a (s:state)  =
    match a with 
    | N(x) -> x
    | V(x) -> match s.TryFind x with 
                | Some y -> y
                | None -> 0
    | Add(x, y) -> arithEvalState(x)(s) + arithEvalState(y)(s)
    | Sub(x, y) -> arithEvalState(x)(s) - arithEvalState(y)(s)
    | Mul(x, y) -> arithEvalState(x)(s) * arithEvalState(y)(s)