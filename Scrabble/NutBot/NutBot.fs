namespace NutBot

open MultiSet
open ScrabbleState
open Dictionary

module NutBot =
    let toMoveItem x y id c p = ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))

    type Direction = 
        | Down of int * int
        | Right of int * int
        // | Left of int * int
        // | Up of int * int

    // let moveInDirection direction : Direction -> Direction
    let moveInDirection direction = 
        match direction with 
        | Down(x, y) -> Down(x, y+1)
        | Right(x, y) -> Right(x+1, y)
        // | Left(x, y) -> Left(x-1, y)
        // | Up(x,y) -> Up(x, x-1)
        
    // let coords direction : Direction -> int * int
    let coords direction = 
        match direction with 
            | Down(x, y) -> (x, y)
            | Right(x, y) -> (x, y)
            // | Left(x,y) -> (x,y)
            // | Up(x,y) -> (x,y)   
    
    let xCoord direction = 
        match direction with 
            | Down(x, y) -> x
            | Right(x, y) -> x
            // | Left(x,y) -> x
            // | Up(x,y) -> x

    let yCoord direction = 
        match direction with 
            | Down(x, y) -> y
            | Right(x, y) -> y
            // | Left(x,y) -> y
            // | Up (x,y) -> y


    // let perpendicularDirection direction : Direction -> Direction
    let perpendicularDirection direction = 
        match direction with 
            | Down(x, y) -> Right(x, y)
            | Right(x, y) -> Down(x, y) 
      
    let isUsedSquare =
        function 
        | Hole
        | UnusedSquare -> false
        | UsedSquare -> true

    let perpendicularWord loc st = 
        let rec findTop l = 
            match l with
            | Down(x, y) -> 
                if (query st (Down(x-1, y) |> coords) |> isUsedSquare) then
                    findTop <| Down(x-1, y)
                else 
                    l
            | Right(x, y) -> 
                if (query st (Right(x, y-1) |> coords) |> isUsedSquare) then
                    findTop <| Right(x, y-1)
                else 
                    l
        let rec getWord c acc = 
            match c with
            | Down(x, y) -> 
                match query st (x, y) with
                | Hole
                | UnusedSquare _ -> acc
                | UsedSquare (_, c, _) -> getWord (Down(x+1, y)) acc + string c 
            | Right(x, y) -> 
                match query st (x, y) with
                | Hole
                | UnusedSquare _ -> acc
                | UsedSquare (_, c, _) -> getWord (Down(x, y+1)) acc + string c                 
        
        lookup (getWord (findTop loc) "") (dictionary st) 

    let rec FindPlay location dict hand st acc =
        match isEnd dict, query st (coords location) with
        | (true, _) 
        | _, UnusedSquare _ when (MultiSet.size hand) = 0u ->
            if (query st (moveInDirection location |> coords) |> isUsedSquare) then
                []
            else
                acc            
        | _, Hole -> []
        | _, UsedSquare (id, c, v) ->
            match contains (string c) (dict) with 
            | Some d -> FindPlay (moveInDirection location) d hand st acc
            | None -> [] 
        | _, UnusedSquare f ->
            let perms = MultiSet.permutations hand
            let plays = List.fold (fun l (id, rest) -> 
                match contains (getChar st id |> string ) (dict) with
                    | Some d -> if perpendicularWord location st then 
                                    (FindPlay (moveInDirection location) d rest st ((toMoveItem (xCoord location) (yCoord location) (id) (getChar st id) (getPoints st id))::acc))::l
                                else 
                                    []::l                                
                    | None -> []::l) [] (perms)
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
        let moves = Map.fold (fun acc key v -> 
            (FindPlay (Right(fst(key), snd(key))) (dictionary st) (hand st) (st) [])
                ::(FindPlay (Down(fst(key), snd(key))) (dictionary st) (hand st) (st) [])::acc) [] (usedTiles st)
        let longestMove = List.maxBy (fun l -> List.length l) moves                                       
        longestMove

        
                //:: (FindPlay (Left(fst(key), snd(key))) (dictionary st) (hand st) (st) [])
                //:: (FindPlay (Up(fst(key), snd(key))) (dictionary st) (hand st) (st) [])
                    