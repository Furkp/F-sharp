module Eval

    open StateMonad
    open Parser

    (* Code for testing *)
    
    let hello = [
        ('H', 4)
        ('E', 1)
        ('L', 1)
        ('L', 1)
        ('O', 1)]

    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    
    let emptyState = mkState [] [] []

    let add a b = 
        a >>= (fun v1 -> 
            b >>= (fun v2 -> ret (v1+v2)))

    let div a b = 
        a >>= (fun v1 -> 
            b >>= (fun v2 -> 
                match v2 with
                | 0 -> fail DivisionByZero
                | n -> ret (v1/v2)))

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

    let sub a b = 
        a >>= (fun v1 -> 
            b >>= (fun v2 -> ret (v1-v2)))

    let mul a b = 
        a >>= (fun v1 -> 
            b >>= (fun v2 -> ret (v1*v2)))

    let md a b = 
        a >>= (fun v1 -> 
            b >>= (fun v2 -> 
                match v2 with
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
            | AEq(x, y) -> 
                arithEval x >>= (fun v1 -> 
                    arithEval y >>= (fun v2 -> ret (v1 = v2)))
            | ALt(x, y) -> 
                arithEval x >>= (fun v1 -> 
                    arithEval y >>= (fun v2 -> ret (v1 < v2)))
            | Not(x) -> boolEval x >>= fun v -> ret (not v)     
            | Conj(x, y) -> 
                boolEval x >>= (fun v1 -> 
                    boolEval y >>= (fun v2 -> ret (v1 && v2)))
            | IsVowel(x) -> charEval x >>= fun v -> ret (isVowel v)
            | IsConsonant(x) -> charEval x >>= fun v -> ret (isConsonant v)

    // type stm =                (* statements *)
    // | Declare of string       (* variable declaration *)
    // | Ass of string * aExp    (* variable assignment *)
    // | Skip                    (* nop *)
    // | Seq of stm * stm        (* sequential composition *)
    // | ITE of bExp * stm * stm (* if-then-else statement *)
    // | While of bExp * stm     (* while statement *)

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
                        push >>>= stmntEval stm2 >>>= pop)
            | While(guard, stm) -> boolEval guard >>= (fun v -> 
                    if(v) then
                        push >>>= stmntEval stm >>>= stmntEval (While(guard, stm)) >>>= pop
                    else
                        ret ())

(*
(* Part 3 (Optional) *)
*)
    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let rec arithEval2 a = 
        match a with
            | N x -> prog { return x }
            | V x -> prog { return! lookup x }
            | WL -> prog { return! wordLength }
            | PV x -> prog { 
                let! ax = arithEval2 x 
                return! pointValue ax }
            | Add(x, y) -> prog { return! add (arithEval2 x) (arithEval2 y) }
            | Sub(x, y) -> prog { return! sub (arithEval2 x) (arithEval2 y) }
            | Mul(x, y) -> prog { return! mul (arithEval2 x) (arithEval2 y) }
            | Div(x, y) -> prog { return! div (arithEval2 x) (arithEval2 y) }
            | Mod(x, y) -> prog { return! md (arithEval2 x) (arithEval2 y) }
            | CharToInt(x) -> prog {
                let! c = charEval2 x
                return int (c)
            }

    and charEval2 c = 
        match c with
            | C(x) -> prog { return x }
            | CV(x) -> prog {
                let! a = arithEval2 x
                return! characterValue a}
            | ToUpper(x) -> prog {
                let! c = charEval x
                return System.Char.ToUpper c }
            | ToLower(x) -> prog {
                let! c = charEval x
                return System.Char.ToLower c }
            | IntToChar(x) -> prog {
                let! c = arithEval2 x
                return char c }
            
    and boolEval2 b = 
            match b with 
                | TT -> prog { return true }               
                | FF -> prog { return false }    
                | AEq(x, y) ->  prog { 
                    let! ax = arithEval2 x
                    let! ay = arithEval2 y 
                    return (ax = ay) }
                | ALt(x, y) -> prog { 
                    let! ax = arithEval2 x
                    let! ay = arithEval2 y 
                    return (ax < ay) }
                | Not(x) -> prog { 
                    let! b = boolEval2 x
                    return not b }
                | Conj(x, y) -> prog { 
                    let! ax = boolEval2 x
                    let! ay = boolEval2 y 
                    return (ax && ay) }
                | IsVowel(x) -> prog { 
                    let! c = charEval2 x
                    return isVowel c }
                | IsConsonant(x) -> prog { 
                    let! c = charEval2 x
                    return isConsonant c }


    let rec stmntEval2 stm : SM<unit> = 
        match stm with
            | Declare(x) -> prog { do! declare x }  
            | Skip -> prog { return () }  
            | Ass(x, a) -> prog { 
                let! av = arithEval2 a
                do! update x av
             }  
            | Seq(stm1, stm2) -> prog { 
                do! stmntEval2 stm1 
                do! stmntEval2 stm2 }
            | ITE(guard, stm1, stm2) -> prog {
                let! b = boolEval2 guard
                if(b) then
                    do! push
                    do! stmntEval2 stm1 
                    do! pop
                else
                    do! push
                    do! stmntEval2 stm2 
                    do! pop}
            | While(guard, stm) -> prog {
                let! b = boolEval2 guard
                if(b) then
                    do! push
                    do! stmntEval2 stm
                    do! stmntEval2 <| While(guard, stm)
                    do! pop
                else
                    return ()}

(* Part 4 (Optional) *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> int

    let stmntToSquareFun stm : squareFun = 
        (fun w pos acc -> 
            (stmntEval2 stm) >>>= lookup "_result_" |> 
            evalSM (mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] w ["_pos_"; "_acc_"; "_result_"]) |>
            getResult)
        
    type coord = int * int

    // type boardFun = coord -> Result<squareFun option, Error> 
    type boardFun = coord -> Map<int, squareFun> option
    
    let stmntToBoardFun stm (m: Map<int, Map<int, squareFun>>) : boardFun = 
        (fun (x,y) ->
            prog{
                do! stmntEval2 stm
                let! id = lookup "_result_"
                return m.TryFind id
            } |> 
                evalSM (mkState [("_x_", x); ("_y_", y); ("_result_", 0)] [] ["_x_"; "_y_"; "_result_"]) |> function
                | Success x -> x
                | Failure err -> failwith (sprintf "Error: %A" err)) 
        
                
    
    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
        placed        : Map<int * int, (int * char)>
    }

    // type squareProg = Map<int, string>
    // type boardProg = {
    //     prog       : stm;
    //     squares    : Map<int, Map<int, squareFun>>
    // }

    // let mkBoard c defaultSq boardStmnt ids = 
    //    { center = c;
    //      defaultSquare = stmntToSquareFun defaultSq;
    //      squares = stmntToBoardFun boardStmnt (List.map (fun (x, y) -> (x, stmntToSquareFun y)) ids |> Map.ofList)}
