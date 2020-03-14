// 5.1

let sum m n = 
    let rec aux m acc =
        function
        | 0 -> acc + m
        | n -> aux m (m + n + acc) (n-1)
    aux m 0 n

// 5.2

let length lst =
    let rec aux acc =
        function
        | [] -> acc
        | _::xs -> aux (acc+1) xs
    aux 0 lst

// 5.3

let foldBack f lst acc =
    let rec aux f acc c =
        function
        | [] -> c acc
        | x::xs -> aux f acc (fun xv -> c(f x xv)) xs
    aux f acc id lst

// 5.4

let factA x =
    let rec aux acc =
        function
        | 0 -> acc
        | x -> aux (x * acc) (x - 1)
    aux 1 x

let factC x =
    let rec aux c =
        function
        | 0 -> c 1
        | x -> aux (fun xv -> c x*xv) (x-1)
    aux id x    

(*
    They both seem to compute instantly which makes it hard to compare.
    Using #time on the functions returns
        Real: 00:00:00.000, CPU: 00:00:00.000
*)

// 5.5

let fibW x =
    let mutable res1 = 0
    let mutable res2 = 1
    let mutable i = 1
    while (i <= x) do
        let temp = res1
        res1 <- res2
        res2 <- temp + res2
        i <- i + 1
    res1

let fibA x =
    let rec aux acc acc2 =
        function
        | 0 -> acc
        | x -> aux acc2 (acc + acc2) (x-1)
    aux 0 1 x 

let fibC x = 
    let rec aux c = 
        function
        | 0 -> c 0
        | 1 -> c 1
        | x -> aux (fun xv -> aux (fun yv -> c(xv + yv)) (x-2)) (x-1)
    aux id x

(*
    fibW and fibA seem to be computing at the same speed
    fibC on the other hand is much slower and seems to grow exponentially
*)

// 5.6

let rec bigListK c =
    function
    | 0 -> c []
    | n -> bigListK (fun res -> 1 :: c res) (n - 1)

(*
    The continuation is used wrong which makes every cons add to the stack
    This would have been a better continuation:
        (fun res -> c(1 :: res)) 
*)

// 5.7

type word = (char * int) list
type state = Map<string, int>

type aExp =
    | N of int              (* Integer literal *)
    | V of string           (* Variable reference *)
    | WL                    (* Word length *)
    | PV of aExp            (* Point value lookup at word index *)
    | Add of aExp * aExp    (* Addition *)
    | Sub of aExp * aExp    (* Subtraction *)
    | Mul of aExp * aExp    (* Multiplication *)
    | CharToInt of cExp     (* NEW: Cast to integer *)

and cExp =
   | C  of char             (* Character literal *)
   | CV of aExp             (* Character lookup at word index *)
   | ToUpper of cExp        (* Convert character to upper case *)
   | ToLower of cExp        (* Convert character to lower case *)
   | IntToChar of aExp      (* NEW: Cast to character *)

let rec arithEvalSimple a (w:word) (s:state) =
    match a with
    | N(x) -> x
    | V(x) -> match s.TryFind x with 
                | Some y -> y
                | None -> 0
    | WL -> w.Length
    | PV(x) -> snd(w.[arithEvalSimple x w s])
    | Add(x, y) -> arithEvalSimple x w s + arithEvalSimple y w s
    | Sub(x, y) -> arithEvalSimple x w s - arithEvalSimple y w s
    | Mul(x, y) -> arithEvalSimple x w s * arithEvalSimple y w s
    | CharToInt(x) -> int (charEvalSimple x w s)

and charEvalSimple c w s =
    match c with
    | C(x) -> x
    | CV(x) -> fst(w.[arithEvalSimple x w s])
    | ToUpper(x) -> System.Char.ToUpper (charEvalSimple x w s)
    | ToLower(x) -> System.Char.ToLower (charEvalSimple x w s)
    | IntToChar(x) -> char (arithEvalSimple x w s)


// 5.8

let rec arithEvalTail a (w:word) (s:state) c =
    match a with
    | N(x) -> c x
    | V(x) -> match s.TryFind x with 
                | Some y -> c y
                | None -> c 0
    | WL -> c w.Length
    | PV(x) -> arithEvalTail x w s (fun xv -> c (snd(w.[xv])))
    | Add(x, y) -> arithEvalTail x w s (fun xv -> arithEvalTail y w s (fun yv -> c(xv + yv)))
    | Sub(x, y) -> arithEvalTail x w s (fun xv -> arithEvalTail y w s (fun yv -> c(xv - yv)))
    | Mul(x, y) -> arithEvalTail x w s (fun xv -> arithEvalTail y w s (fun yv -> c(xv * yv)))
    | CharToInt(x) -> charEvalTail x w s (fun xv -> c (int xv))

and charEvalTail e w s c = 
    match e with
    | C(x) -> c x
    | CV(x) -> arithEvalTail x w s (fun xv -> c (fst(w.[xv])))
    | ToUpper(x) -> charEvalTail x w s (fun xv -> c (System.Char.ToUpper xv))
    | ToLower(x) -> charEvalTail x w s (fun xv -> c (System.Char.ToUpper xv))
    | IntToChar(x) ->  arithEvalTail x w s (fun xv -> c (char xv))

let arithEval a w s = arithEvalTail a w s id
let charEval c w s  = charEvalTail c w s id

// 5.9

let odds = Seq.filter (fun n -> n%2=1) (Seq.initInfinite id)

// 5.10

let facts = Seq.initInfinite factA