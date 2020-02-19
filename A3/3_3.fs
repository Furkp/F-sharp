module aExp

type word = (char * int) list
type squareFun = word -> int -> int -> int
type state = Map<string, int>

type aExp =
    | N of int
    | V of string
    | WL
    | PV of aExp
    | Add of aExp * aExp
    | Sub of aExp * aExp
    | Mul of aExp * aExp

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_")
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_")
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_")

let arithDoubleWordScore = N 2 .*. V "_acc_"
let arithTripleWordScore = N 3 .*. V "_acc_"

let hello = 
    [
    ('H', 4)
    ('E', 1)
    ('L', 1)
    ('L', 1)
    ('O', 2)]:word

let rec arithEval a (w:word) (s:state) = 
    match a with 
    | N(x) -> x
    | V(x) -> match s.TryFind x with 
                | Some y -> y
                | None -> 0
    | WL -> w.Length
    | PV(x) -> snd(w.Item(arithEval(x)(w)(s)))
    | Add(x, y) -> arithEval(x)(w)(s) + arithEval(y)(w)(s)
    | Sub(x, y) -> arithEval(x)(w)(s) - arithEval(y)(w)(s)
    | Mul(x, y) -> arithEval(x)(w)(s) * arithEval(y)(w)(s)

type cExp =
   | C  of char      (* Character value *)
   | ToUpper of cExp (* Converts lower case to upper case character, non-characters unchanged *)
   | ToLower of cExp (* Converts upper case to lower case character, non characters unchanged *)
   | CV of aExp      (* Character lookup at word index *)

let rec charEval c (w:word) (s:state) = 
    match c with
    | C(x) -> x
    | ToUpper(x) -> System.Char.ToUpper (charEval x w s)
    | ToLower(x) -> System.Char.ToLower (charEval x w s)
    | CV(x) -> fst(w.Item(arithEval x w s))

type bExp =             
   | TT                   (* true *)
   | FF                   (* false *)

   | AEq of aExp * aExp   (* numeric equality *)
   | ALt of aExp * aExp   (* numeric less than *)

   | Not of bExp          (* boolean not *)
   | Conj of bExp * bExp   (* boolean conjunction *)

   | IsVowel of cExp      (* check for vowel *)
   | IsConsonant of cExp  (* check for constant *)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
   
let (.=.) a b = AEq (a, b)   
let (.<.) a b = ALt (a, b)   
let (.<>.) a b = ~~(a .=. b)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

let isVowel c = 
    match System.Char.ToLower(c) with
    | 'a' |'e' |'i' | 'o' | 'u' -> true
    | _ -> false

let isConsonant c =
    System.Char.IsLetter(c) && not (isVowel(c))

let rec boolEval b w s = 
    match b with
    | TT -> true
    | FF -> false
    | AEq(x, y) -> arithEval x w s = arithEval y w s
    | ALt(x, y) -> arithEval x w s < arithEval y w s
    | Not(x) -> not (boolEval x w s)
    | Conj(x, y) -> boolEval x w s && boolEval y w s
    | IsVowel(x) -> isVowel(charEval x w s)
    | IsConsonant(x) -> isConsonant(charEval x w s)

type stmnt =
   | Skip                        (* does nothing *)
   | Ass of string * aExp        (* variable assignment *)
   | Seq of stmnt * stmnt        (* sequential composition *)
   | ITE of bExp * stmnt * stmnt (* if-then-else statement *)    
   | While of bExp * stmnt       (* while statement *)

let rec evalStmnt stm w (s:state) =
    match stm with
    | Skip -> s
    | Ass(x, a) -> s.Add(x, arithEval a w s)
    | Seq(stm1, stm2) -> evalStmnt stm2 w (evalStmnt stm1 w s)
    | ITE(guard, stm1, stm2) -> if(boolEval guard w s) then
                                    evalStmnt stm1 w s
                                else
                                    evalStmnt stm2 w s
    | While(guard, stm) -> if(boolEval guard w s) then
                                    evalStmnt (While(guard, stm)) w (evalStmnt stm w s)
                                else
                                    s

let stmnt2SquareFun stm = 
    let f w pos acc = (evalStmnt stm w (Map.ofList[("_pos_", pos); ("_acc_", acc)])).Item("_result_")
    f

let singleLetterScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithSingleLetterScore))
let doubleLetterScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithDoubleLetterScore))
let tripleLetterScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithTripleLetterScore))

let doubleWordScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithDoubleWordScore))
let tripleWordScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithTripleWordScore))

let oddConsonants : squareFun = 
    stmnt2SquareFun 
        (Seq (Ass ("_result_", V "_acc_"),
              While (V "i" .<. WL,
                     Seq(
                         ITE (IsConsonant (CV (V "i")),
                              Ass ("_result_", V "_result_" .*. N -1),
                              Skip),
                         Ass ("i", V "i" .+. N 1)))))

type square2 = (int * stmnt) list

let SLS = [(0, Ass ("_result_", arithSingleLetterScore))]
let DLS = [(0, Ass ("_result_", arithDoubleLetterScore))]
let TLS = [(0, Ass ("_result_", arithTripleLetterScore))]

let DWS = [(1, Ass ("_result_", arithDoubleWordScore))] @ SLS
let TWS = [(1, Ass ("_result_", arithTripleWordScore))] @ SLS

// let calculatePoints2 : square2 list -> word -> int = failwith "not implemented"
