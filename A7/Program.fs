// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open JParsec
open Parser
open TextParser
open Ass7.ImpParser

[<EntryPoint>]
let main argv =
    (* Uncomment these tests as you need them *)
    
    (*
    runTextParser pif "if" |> printResult
    runTextParser pif "if-some random text" |> printResult
    runTextParser pif "ithen-some random text" |> printResult

    runTextParser (pif .>*>. pthen) "if    then" |> printResult
    runTextParser (pif .>*>. pthen) "ifthen" |> printResult
    runTextParser (pif .>*>. pthen) "if   then    " |> printResult
    runTextParser (pif .>*>. pthen) "    if   then" |> printResult
    runTextParser (pif .>*>. pthen .>*>. pelse) "if   then    else" |> printResult

    runTextParser (parenthesise pint)  "(    5    )" |> printResult
    runTextParser (parenthesise pthen) "(    then )" |> printResult
    runTextParser (parenthesise pint)  "(    x    )" |> printResult
    runTextParser (parenthesise pint)  "(  5    x )" |> printResult

    runTextParser pid "x" |> printResult
    runTextParser pid "x1" |> printResult
    runTextParser pid "1x" |> printResult
    runTextParser pid "longVariableName" |> printResult

    printfn "\nTesting unop"
    runTextParser (unop (pchar '-') pint) "-5"                |> printResult
    runTextParser (unop (pchar '-') pint) "-     5"           |> printResult
    runTextParser (unop (pchar '-') pint |>> ( * ) (-1)) "-5" |> printResult
    runTextParser (unop (pchar '-') pint) "-     x"           |> printResult

    printfn "\nTesting binop"
    runTextParser (binop (pchar '+') pint pint) "5 +  7" |> printResult
    runTextParser (binop (pchar '+') pint pid) "5+var"   |> printResult
    runTextParser (binop (pchar '+') pint pint |>>
                   (fun (a, b) -> a + b)) "5 +  7"       |> printResult

    printfn "\nTesting arithmetic expressions"
    runTextParser AexpParse "4" |> printResult
    runTextParser AexpParse "x2" |> printResult
    runTextParser AexpParse "5 + y * 3" |> printResult
    runTextParser AexpParse "(5 - y) * -3" |> printResult
    runTextParser AexpParse "pointValue (x % 4) / 0" |> printResult

    printfn "\nTesting character expressions"
    runTextParser CexpParse "'x'" |> printResult
    runTextParser CexpParse "toLower (toUpper( 'x'))" |> printResult
    runTextParser CexpParse "intToChar (charToInt (' '))" |> printResult
    runTextParser AexpParse "charToInt (charValue (pointValue (5)))" |> printResult

    printfn "\nTesting boolean expressions"
    runTextParser BexpParse "true" |> printResult
    runTextParser BexpParse "false" |> printResult
    runTextParser BexpParse "5 > 4 \\/ 3 >= 7" |> printResult
    runTextParser BexpParse "~false" |> printResult
    runTextParser BexpParse "(5 < 4 /\ 6 <= 3) \/ ~false" |> printResult
    runTextParser BexpParse "(5 < 4 \/ 6 <= 3) \/ ~true" |> printResult
*)
    printfn "\nTesting statements"
    runTextParser stmParse "x := 5" |> printResult
    runTextParser stmParse "declare myVar" |> printResult
    runTextParser stmParse "declaremyVar" |> printResult
    runTextParser stmParse "declare x; x := 5" |> printResult
    runTextParser stmParse "declare x; x := 5  ;y:=7" |> printResult
    runTextParser stmParse "if (x < y) then { x := 5 } else { y := 7 }" |> printResult
    runTextParser stmParse "if (x < y) then { x := 5 }" |> printResult
    runTextParser stmParse "while (true) do {x5 := 0} " |> printResult
    runParserFromFile stmParse "./Factorial.txt" |> printResult
    
    0 // return an integer exit code
