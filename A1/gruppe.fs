// MISC

// 1.1
let sqr x = x*x 

// 1.2
let pow x n = System.Math.Pow(x, n)

// 1.3
let rec sum =
    function
    | 0 -> 0
    | n -> n + sum (n-1)

// 1.4
let rec fib = 
    function
    | 0 -> 0
    | 1 -> 1
    | n -> fib (n-1) + fib (n-2)

// 1.5
let rec sum2 =
    function
    | m, 0 -> m
    | m, n -> m + n + sum2 (m, n-1)
 
(* 1.6
   (System.Math.PI, fact -1) : float * int
   fact(fact 4) : int -> int -> int
   power(System.Math.PI, fact 2) : float * (int -> int) -> float
   (power, fact) : (float * int -> float) * (int -> int)
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

// 1.8
let dup s = ""+s+s

// 1.9
let rec dupn (s:string) (n:int) = 
    match n with
    | 0 -> ""
    | 1 -> s
    | n -> s + dupn s (n-1)

// 1.10
let timediff (ah, am) (bh, bm) =
    (bh*60+bm)-(ah*60+am)

// 1.11
let minutes (h, m) =
    timediff(0,0)(h,m)

// 1.12
let rec bin = 
    function
    | n, 0 -> 1
    | n, k -> if(n = k) then 1 else bin(n-1,k-1) + bin(n-1,k)

(* 1.13
    1. Determine the type of f
    int*int->int

    2. For which arguments does the evaluation of f terminate?
    (0,y)

    3. Write the evaluation steps for f(2, 3).
    f(2,3) 
        ~> f(2-1, 2*6) 
        ~> f(1, 6) 
        ~> f(1-1, 6*1) 
        ~> f(0, 6) 
        ~> 6

    4. What is the mathematical meaning of f(x, y) ?
    f(0, y) := y
    f(x, y) := f(x-1, x*y)
*)

(* 1.15
    1. What is the type of test?
    bool*int->int

    2. What is the result of evaluating test(false, fact(−1)) ?
    Since F# uses eager evaluation fact(-1) results in a stackoverflow

    3. Compare this with the result of evaluating if false then fact -1 else 0
    This evaluates to 0. Fact(-1) is never reached as it's in the "then" section   
*)

// 1.16
let curry f x y = f(x, y)
let uncurry f (x, y) = f x y

// SCRABBLE

// 1.17
let isVowel c = 
    match System.Char.ToLower(c) with
    | 'a' |'e' |'i' | 'o' | 'u' -> true
    | _ -> false

// 1.18
let isConsonant c =
    System.Char.IsLetter(c) && not (isVowel(c))

// 1.19
let empty (c, x) =
    let f n = (c, x)
    f

// 1.20
let add pos (c, v) word =
    let f n = 
        if(n=pos) then (c,v) else word(n)
    f

// 1.21
let hello =
    empty(char 0, 0) |> 
    add 0 ('H', 4) |> 
    add 1 ('E', 1) |> 
    add 2 ('L', 1) |> 
    add 3 ('L', 1) |> 
    add 4 ('O', 2)

// 1.22
let singleLetterScore word pos =
    snd(word(pos))
let doubleLetterScore word pos =
    snd(word(pos)) * 2
let tripleLetterScore word pos =
    snd(word(pos)) * 3