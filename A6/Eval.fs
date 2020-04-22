module Eval

    open StateMonad

    (* Code for testing *)
    
    let hello = [
        ('H', 4)
        ('E', 1)
        ('L', 1)
        ('L', 1)
        ('O', 1)]

    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    
    let emptyState = mkState [] [] []

    let add a b = a >>= 
        (fun v1 -> b >>= 
            (fun v2 -> ret (v1+v2)))

    let div a b = a >>= 
        (fun v1 -> b >>= 
            (fun v2 -> 
                match v2 with
                | 0 -> fail DivisionByZero
                | n -> ret (v1/v2)))
     
    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsConsonant of cExp  (* check for constant *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let sub a b = a >>= 
        (fun v1 -> b >>= 
            (fun v2 -> ret (v1-v2)))

    let mul a b = a >>= 
        (fun v1 -> b >>= 
            (fun v2 -> ret (v1*v2)))

    let md a b = a >>= 
        (fun v1 -> b >>= 
            (fun v2 -> match v2 with
                | 0 -> fail DivisionByZero
                | n -> ret (v1/v2)))
            
    let isVowel c = 
        match System.Char.ToLower(c) with
        | 'a' |'e' |'i' | 'o' | 'u' -> true
        | _ -> false

    let isConsonant c =
        System.Char.IsLetter(c) && not (isVowel(c))

    let rec arithEval a : SM<int> = 
        match a with
            | N x -> ret x
            | V x -> lookup x
            | WL -> wordLength 
            | PV x -> arithEval x >>= pointValue
            | Add(x, y) -> add (arithEval x) (arithEval y)
            | Sub(x, y) -> sub (arithEval x) (arithEval y)
            | Mul(x, y) -> mul (arithEval x) (arithEval y)
            | Div(x, y) -> div (arithEval x) (arithEval y)
            | Mod(x, y) -> md (arithEval x) (arithEval y)
            | CharToInt(x) -> (charEval x) >>= (fun v -> ret (int v)) 
    
    and charEval c : SM<char> = 
        match c with
            | C(x) -> ret x
            | CV(x) -> arithEval x >>= characterValue
            | ToUpper(x) -> charEval x >>= (fun v -> ret (System.Char.ToUpper v))
            | ToLower(x) -> charEval x >>= (fun v -> ret (System.Char.ToLower v))
            | IntToChar(x) -> arithEval x >>= (fun v -> ret (char v))
   
    and boolEval b : SM<bool> = 
        match b with 
            | TT -> ret true               
            | FF -> ret false    
            | AEq(x, y) -> arithEval x >>= 
                (fun v1 -> arithEval y >>= 
                    (fun v2 -> ret (v1 = v2)))
            | ALt(x, y) -> arithEval x >>= 
                (fun v1 -> arithEval y >>= 
                    (fun v2 -> ret (v1 < v2)))
            | Not(x) -> boolEval x >>= fun v -> ret (not v)     
            | Conj(x, y) -> boolEval x >>= 
                (fun v1 -> boolEval y >>= 
                    (fun v2 -> ret (v1 && v2)))
            | IsVowel(x) -> charEval x >>= fun v -> ret (isVowel v)
            | IsConsonant(x) -> charEval x >>= fun v -> ret (isConsonant v)

    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> = 
        match stmnt with
            | Declare(x) -> declare x
            | Skip -> ret ()
            | Ass(x, a) -> arithEval a >>= update x
            | Seq(stm1, stm2) -> stmntEval stm1 >>>= stmntEval stm2
            | ITE(guard, stm1, stm2) -> boolEval guard >>= (fun v -> 
                    if(v) then
                        push >>>= stmntEval stm1 >>>= pop
                    else
                        push >>>= stmntEval stm1 >>>= pop)
            | While(guard, stm) -> boolEval guard >>= (fun v -> 
                    if(v) then
                        push >>>= stmntEval (While(guard, stm)) >>>= pop
                    else
                        ret ())

(*
(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = failwith "Not implemented"


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
*)