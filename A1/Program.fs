// Learn more about F# at http://fsharp.org

open System

module misc =
    let sqr x = x*x
    let pow x n = System.Math.Pow(x, n)
    let rec sum =
        function
        | 0 -> 0
        | n -> n + sum (n-1)
    let rec fib = 
        function
        | 0 -> 0
        | 1 -> 1
        | n -> fib (n-1) + fib (n-2)
    let rec sum2 =
        function
        | m, 0 -> m
        | m, n -> m + n + sum2 (m, n-1)
    let a = 5
    let f a = a + 1
    let g b = (f b) + a
    let dup s = ""+s+s
    let rec dupn = 
        function
        | (s, 1) -> s+""
        | (s, n) -> s + dupn(s, n-1)
    let timediff (ah, am) (bh, bm) =
        (bh*60+bm)-(ah*60+am)
    let minutes (h, m) =
        timediff(0,0)(h,m)
    let rec bin = 
        function
        | n, 0 -> 1
        | n, k -> if(n = k) then 1 else bin(n-1,k-1) + bin(n-1,k)
    let curry f x y = f(x, y)
    let uncurry f (x, y) = f x y
module scrabble = 
    let isVowel c = 
        match System.Char.ToLower(c) with
        | 'a' |'e' |'i' | 'o' | 'u' -> true
        | _ -> false
    let isConsonant c =
        System.Char.IsLetter(c) && not (isVowel(c))
    let empty (c, x) =
        let f n = (c, x)
        f
    let add pos (c, v) word =
        let f n = 
            if(n=pos) then (c,v) else word(n)
        f
    let hello =
        empty(char 0, 0) |> 
        add 0 ('H', 4) |> 
        add 1 ('E', 1) |> 
        add 2 ('L', 1) |> 
        add 3 ('L', 1) |> 
        add 4 ('O', 2)
    let singleLetterScore word pos =
        snd(word(pos))
    let doubleLetterScore word pos =
        snd(word(pos)) * 2
    let tripleLetterScore word pos =
        snd(word(pos)) * 3

[<EntryPoint>]
let main argv =
    (* 1.6
       float * int
       int
       float
       (float*int->float) * (int->int)
    *) 
    (* 1.7
        let a = 5;;
            val a : int = 5
        let f a = a + 1;;
            val f 3 : int = 4
        let g b = (f b) + a;;
            val g 3 : int = 9
        [a -> 5; f -> 4; g -> 9]
    *)
    (* 1.13
        int*int->int
        (0,y)
        f(2,3) ~> f(2-1, 2*6) ~> f(1, 6) ~> f(1-1, 6*1) ~> f(0, 6) ~> 6
        f(0, y) := y
        f(x, y) := f(x-1, x*y)
    *)
    (* 1.15
        bool*int->int
        Since F# uses eager evalueation fact(-1) results in a stackoverflow
        This evaluates to 0. Fact(-1) is never reached as it's in the "then" section   
    *)
    0 // return an integer exit code
