// 2.1

let rec downto1 n =
    if n > 0 then n :: downto1(n-1)
    else []

let rec downto2 =
    function
    | 0 -> []
    | n -> n::downto2 (n-1)

// 2.2

let rec removeOddIdx =
    function
    | [] -> []
    | x::x2::xs -> x::removeOddIdx(xs)
    | x::xs -> x::[]

// 2.3

let rec combinePair =
    function
    | x::x2::xs -> (x,x2)::combinePair xs
    | x -> []

// 2.4

type complex = float * float 

let mkComplex a b = (a, b):complex

let complexToPair =
    function
    | ((x, y):complex) -> (x, y)

let (|+|) (a) (b) = 
    let a1, a2 = complexToPair(a)
    let b1, b2 = complexToPair(b)
    (a1 + b1, a2 + b2):complex
    
let (|*|) (a) (b) = 
    let a1, a2 = complexToPair(a)
    let b1, b2 = complexToPair(b)
    (a1 * b1 - a2 * b2, a2 * b1 + a1 * b2):complex

let (|-|) (a) (b) = 
    let b1, b2 = complexToPair(b)
    a |+| mkComplex -b1 -b2
  

let (|/|) (a) (b) = 
    let b1, b2 = complexToPair(b)
    let den = (b1*b1) + (b2*b2)
    a |*| mkComplex (b1/den) (-b2/den)

// 2.7

let explode1 (s:string) = 
    List.ofArray <| s.ToCharArray()
    
let rec explode2 =
    function
    | "" -> []
    | s -> s.Chars(0) :: explode2(s.Remove(0, 1))

// 2.8

let implode (cs:char list) =
    List.foldBack(
        fun (a:char) (e:string) -> a.ToString() + e
    ) cs ""

let implodeRev (cs:char list) =
    List.fold(
        fun (a:string) (e:char) -> e.ToString() + a
    ) "" cs

// 2.9

let toUpper s = 
    s |> explode2 |> List.map System.Char.ToUpper |> implode
   
// 2.10

let rec ack =
    function
    | 0, n -> n+1
    | m, 0 -> ack(m-1, 1)
    | m, n -> ack(m-1, ack(m, n-1))

// 2.11

let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start)

let timeArg1 f a =
    let start = System.DateTime.Now
    let res = f a
    let finish = System.DateTime.Now
    (res, finish - start)

// 2.12

let downto3 f n e=
    if(n > 0) then
        List.foldBack f [1..n] e 
    else
        e

let fac n = 
    downto3 (*) n 1 

let range g n =  
    List.map g [1..n]


// Scrabble

// 2.13

type word = (char * int) list

let hello = 
    [
    ('H', 4)
    ('E', 1)
    ('L', 1)
    ('L', 1)
    ('O', 2)]:word
    
// 2.14

type squareFun = word -> int -> int -> int

let singleLetterScore:squareFun = fun w pos acc ->
    acc + snd(w.Item(pos))

let doubleLetterScore:squareFun = fun w pos acc ->
    acc + snd(w.Item(pos)) * 2

let tripleLetterScore:squareFun = fun w pos acc ->
    acc + snd(w.Item(pos)) * 3

// 2.15

let doubleWordScore:squareFun = fun w pos acc ->
    acc * 2

let tripleWordScore:squareFun = fun w pos acc ->
    acc * 3

    
// 2.16

// let isVowel c = 
//     match System.Char.ToLower(c) with
//     | 'a' |'e' |'i' | 'o' | 'u' -> true
//     | _ -> false

// let isConsonant c =
//     System.Char.IsLetter(c) && not (isVowel(c))

// let oddConsonants:squareFun = fun w pos acc ->
//     if((List.filter (fun x -> isConsonant(snd(x))) w).Length % 2 = 0) then
//         acc
//     else
//         -1 * acc