namespace NutBot

open MultiSet
open ScrabbleState
open Dictionary

module NutBot =
    let toMoveItem x y id c p = ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))

    type Direction = 
        | Down of int * int
        | Right of int * int

    // let moveInDirection direction : Direction -> Direction
    let moveInDirection direction = 
        match direction with 
        | Down(x, y) -> Down(x, y+1)
        | Right(x, y) -> Right(x+1, y)

    // let coords direction : Direction -> int * int
    let coords direction = 
        match direction with 
            | Down(x, y) -> (x, y)
            | Right(x, y) -> (x, y)    
    
    // let perpendicularDirection direction : Direction -> Direction
    let perpendicularDirection direction = 
        match direction with 
            | Down(x, y) -> Right(x, y)
            | Right(x, y) -> Down(x, y) 
      
    let perpendicularWord = true

    let rec FindPlay location dict hand st acc =
        match dict, query st (coords location) with
        // | (true, _), _ -> [acc]
        | _, Hole -> []
        | _, UsedSquare (id, c, v) ->
            match contains (string c) (dict) with 
            | Some d -> FindPlay (moveInDirection location) d hand st acc
            | None -> [] 
        | _, UnusedSquare f ->
            let plays = List.fold (fun l (c, rest) -> 
                match contains (string c) (dict) with
                    | Some d -> if perpendicularWord then 
                                    (FindPlay (moveInDirection location) d rest st acc)::l
                                else 
                                    []::l                                
                    | None -> []::l) [] (MultiSet.permutations hand)
            let longestPlay = List.maxBy (fun l -> List.length l) plays
            longestPlay


    // let rec FindPlay (location:Direction) (dict:Dictionary) (hand) (board:Board) (acc) =
    //     match dict, lookupLocation (coords location) board with
    //     | (endOfWord, _), UnusedSquare _ when size hand = 0 -> 
    //         if not (IsUsedSquare (moveInDirection location)) then
    //             [acc]
    //         else
    //             []
    //     | _, Hole -> []
    //     | _, UsedSquare (tid, c, v) -> 
    //         match dictionary.TryFind c with
    //         | None -> []
    //         | Some innerDict -> FindPlay (moveInDirection location) innerDict hand board acc
    //     | _, UnusedSquare func -> 
    //         // You will probably want to fold over this
    //         // As it gives multiple different possibilities. 
    //         // For this you would add the result of the recursive call with
    //         // the @ Append symbol.
    //         for (c, restHand) in (FindPermutations Hand) do
    //             match dictionary.TryFind c with
    //             | None -> []
    //             | Some innerDict -> 
    //                 if PerpendicularWord c (coords location) in WholeDict then
    //                     FindPlay (moveInDirection location) innerDict hand board c::acc
    //                 else 
    //                     []
    //         // You probably also want to check if there is a word which does not 
    //         // use all tiles and if so add it to the results.
    let getMove st = 
        let x = 0
        let y = 0
        let id = 0
        let c = 'a'
        let p = 1
        let moves = Map.fold (fun acc key v -> 
            (FindPlay (Right(fst(key), snd(key))) (dictionary st) (hand st) (st) [])
                ::(FindPlay (Down(fst(key), snd(key))) (dictionary st) (hand st) (st) [])
                    ::acc) [] (usedTiles st)
        let longestMove = List.maxBy (fun l -> List.length l) moves                                       
        [(toMoveItem x y id c p)]