// Learn more about F# at http://fsharp.org

open System

module misc =
    let sqr x = x*x

[<EntryPoint>]
let main argv =
    printfn "sqr 2 = %d" (misc.sqr 2)
    0 // return an integer exit code
