module Parser

open System
open FParsec
open ScrabbleUtil

type aExp =
    | N of int              (* Integer literal *)
    | V of string           (* Variable reference *)

    | WL                    (* Word length *)
    | PV of aExp            (* Point value lookup at word index *)

    | Add of aExp * aExp    (* Addition *)
    | Sub of aExp * aExp    (* Subtraction *)
    | Mul of aExp * aExp    (* Multiplication *)
    | Div of aExp * aExp    (* Division *)
    | Mod of aExp * aExp    (* Modulo *)

    | CharToInt of cExp     (* Cast to integer *)

and cExp =
    | C  of char             (* Character literal *)
    | CV of aExp             (* Character lookup at word index *)

    | ToUpper of cExp        (* Convert character to upper case *)
    | ToLower of cExp        (* Convert character to lower case *)

    | IntToChar of aExp      (* Cast to character *)

type bExp =
    | TT                   (* True *)
    | FF                   (* False *)

    | AEq of aExp * aExp   (* Numeric equality *)
    | ALt of aExp * aExp   (* Numeric less than *)

    | Not of bExp          (* Boolean not *)
    | Conj of bExp * bExp  (* Boolean conjunction *)

    | IsVowel of cExp      (* Check for vowel *)
    | IsConsonant of cExp  (* Check for constant *)

type stm =
    | Declare of string       (* NEW: Variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* Nop *)
    | Seq of stm * stm        (* Sequential composition *)
    | ITE of bExp * stm * stm (* If-Then-Else statement *)
    | While of bExp * stm     (* While statement *)

module ImpParser =
    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)

    let (.=.) a b = AEq (a, b)
    let (.<.) a b = ALt (a, b)
    let (.<>.) a b = ~~(a .=. b)                (* numeric inequality *)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)  (* numeric smaller than or equal to *)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

    let whitespaceChar = satisfy Char.IsWhiteSpace
    let pAnyChar = anyChar
    let letterChar = asciiLetter
    let alphaNumeric = asciiLetter <|> digit
    let charListToStr charList = String(List.toArray charList) |> string
    let pint = pint32
    let choice ps = ps |> Seq.map attempt |> choice
    let (<|>) p1 p2 = attempt p1 <|> attempt p2

    let pIntToChar = pstring "intToChar"
    let pPointValue = pstring "pointValue"
    let pCharToInt = pstring "charToInt"
    let pToUpper = pstring "toUpper"
    let pToLower = pstring "toLower"
    let pCharValue = pstring "charValue"
    
    let (.>*>.) p1 p2 =
        p1 .>> spaces >>= (fun r1 -> p2 >>= fun r2 -> preturn (r1, r2)) 

    let (.>*>) p1 p2 = p1 .>*>. p2 |>> fst 
    let (>*>.) p1 p2 = p1 .>*>. p2 |>> snd

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'

    let pid = letterChar <|> pchar '_' .>>. (many alphaNumeric |>> charListToStr) |>> fun (a, b) -> string a+b

    let unop op a = op >*>. a

    let binop op a b = a .>*> op .>*>. b

    let TermParse, tref = createParserForwardedToRef<aExp, unit>()
    let ProdParse, pref = createParserForwardedToRef<aExp, unit>()
    let AtomParse, aref = createParserForwardedToRef<aExp, unit>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"

    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    
    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"

    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"

    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"

    let NegParse = unop (pchar '-') AtomParse  |>> (fun a -> Mul (N(-1), a)) <?> "Neg"

    let WLParse = pstring "wordLength" >*>. pid |>> (fun a -> N a.Length) <?> "WL"
    
    let PVParse = pPointValue >*>. AtomParse |>> PV <?> "PV"
    
    let VParse = pid |>> V <?> "Var"
    
    let NParse   = pint |>> N <?> "Int"   
    
    let ParParse = parenthesise TermParse

    do tref := choice [AddParse; SubParse; ProdParse]
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let AexpParse = TermParse

    let CharParse, cref = createParserForwardedToRef<cExp, unit>()

    let CParse = pchar ''' >>. pAnyChar .>> pchar ''' |>> C <?> "Char"

    let CVParse = pCharValue >*>. parenthesise TermParse |>> CV <?> "CV"
    
    let TUParse = pToUpper >*>. parenthesise CharParse |>> ToUpper <?> "ToUpper"

    let TLParse = pToLower >*>. parenthesise CharParse |>> ToLower <?> "ToLower"

    let ITCParse = pIntToChar >*>. parenthesise TermParse |>> IntToChar <?> "IntToChar"

    let CTIParse = pCharToInt >*>. parenthesise CharParse |>> CharToInt <?> "CharToInt"

    do cref := choice [CVParse; TUParse; TLParse; ITCParse; CParse]
    
    do aref := choice [WLParse; PVParse; CTIParse; VParse; NegParse; NParse; ParParse]

    let CexpParse = CharParse

    let BoolParse, bref = createParserForwardedToRef<bExp, unit>()
    let Bool2Parse, b2ref = createParserForwardedToRef<bExp, unit>()
    let Bool3Parse, b3ref = createParserForwardedToRef<bExp, unit>()

    let TParse = pstring "true" |>> (fun a -> TT) <?> "True"
    let FParse = pstring "false" |>> (fun a -> FF) <?> "False"

    let AndParse = Bool2Parse .>*> pstring "/\\" .>*>. BoolParse |>> Conj <?> "Conj"
    let OrParse =  Bool2Parse .>*> pstring "\\/" .>*>. BoolParse |>> (fun (a, b) -> Not(a), Not(b)) |>> Conj |>> Not <?> "Disj"

    let EqParse =  AtomParse .>*> pstring "=" .>*>. TermParse |>> AEq <?> "AEq"
    let NEqParse =  AtomParse .>*> pstring "<>" .>*>. TermParse |>> AEq |>> Not <?> "Not Equal"
    let LTParse =  AtomParse .>*> pstring "<" .>*>. TermParse |>> ALt <?> "Less than"
    let LTEParse =  AtomParse .>*> pstring "<=" .>*>. TermParse |>> (fun a -> (a |> ALt |> Not), (a |> AEq |> Not |> Not |> Not)) |>> Conj |>> Not <?> "Less than/Equal"
    let GTParse =  AtomParse .>*> pstring ">" .>*>. TermParse |>> (fun a -> ((a |> AEq |> Not), (a |> ALt |> Not))) |>> Conj  <?> "Greater than"
    let GTEParse =  AtomParse .>*> pstring ">=" .>*>. TermParse |>> ALt |>> Not <?> "Greater than/Equal"
    
    let NotParse = pstring "~" >>. BoolParse |>> Not <?> "Not"

    let BParse = parenthesise BoolParse

    do bref := choice [AndParse; OrParse; Bool2Parse]
    do b2ref := choice [EqParse; NEqParse; LTParse; LTEParse; GTParse; GTEParse; Bool3Parse]
    do b3ref := choice [NotParse; BParse; TParse; FParse]

    let BexpParse = BoolParse

    let StmntParse, sref = createParserForwardedToRef<stm, unit>()
    let Stmnt2Parse, s2ref = createParserForwardedToRef<stm, unit>()

    let AssParse = pid .>*> pstring ":=" .>*>. TermParse |>> Ass <?> "Ass"

    let DecParse = pstring "declare" >>. whitespaceChar >*>. pid |>> Declare <?> "Dec" 

    let SeqParse = Stmnt2Parse .>*> pchar ';' .>*>. StmntParse |>> Seq <?> "Seq"

    let bracketise p = pchar '{' >*>. p .>*> pchar '}'

    let ITEParse = pstring "if" >*>. BoolParse .>*> pstring "then" .>*>. 
                    bracketise StmntParse .>*> pstring "else" .>*>. 
                    bracketise StmntParse |>> (fun ((a,b), c) -> (a,b,c) |> ITE) <?> "ITE"

    let ITParse = pstring "if" >*>. BoolParse .>*> pstring "then" .>*>. 
                    bracketise StmntParse |>> (fun (a, b) -> (a,b,Skip) |> ITE) <?> "IT"

    let WhParse = pstring "while" >*>. BoolParse .>*> pstring "do" .>*>. 
                    bracketise StmntParse |>> While <?> "While"

    do sref := choice [SeqParse; Stmnt2Parse]
    do s2ref := choice [ITEParse; ITParse; WhParse; AssParse; DecParse]
    let stmParse = StmntParse 

    let getParserResult s (pr : ParserResult<'a, 'b>) =
        match pr with
        | Success (t, _, _)   -> 
             t
        | Failure (err, _, _) -> 
            let errorStr = sprintf "Failed to parse %s\n\nError:\n%A" s err
            DebugPrint.debugPrint errorStr
            failwith errorStr

    let runTextParser parser text =
        match runParserOnString parser () "" text with
        | Success (stm, _, _) -> stm
        | Failure (err, _, _) -> failwith err  