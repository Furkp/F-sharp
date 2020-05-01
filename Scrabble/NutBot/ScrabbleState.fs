namespace NutBot

open MultiSet
open Dictionary
open Eval
open ScrabbleUtil

module ScrabbleState  = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, and your player numer but it could, potentially, 
    // keep track of other useful
    // information, such as number of players, player turn, etc.

    
    type Board = {
        center : coord 
        usedTiles : Map<int * int, tile>
        boardFunc : boardFun
    }

    type HandState = {
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        dictionary    : Dictionary
        charToInt     : Map<char, uint32>
        board         : Board
    }

    type Direction = 
       // Up and Left are the wrong direction for words and only used when going in reverse with GADDAG
       | Right of int * int
       | Left of int * int
       | Down of int * int
       | Up of int * int

    type SquarePosition =
        | UsedSquare of uint32 * char * int
        | UnusedSquare of Map<int, squareFun>
        | Hole

    let newState pn h d chti c u b = { 
        playerNumber = pn; 
        hand = h;
        dictionary = d;
        charToInt = chti;
        board = {
            center = c;
            usedTiles = u;
            boardFunc = b;
        }
    }
    
   
    let board st = st.board
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    
    let center st = st.board.center
    let usedTiles st = st.board.usedTiles
    let boardFunc st = st.board.boardFunc
    let dictionary st = st.dictionary
    let charToInt st = st.charToInt
    
    let addToHand newPieces st = 
        let addPieces = List.fold (fun acc (id, c) -> MultiSet.add id c acc) (hand st) newPieces        
        newState (playerNumber st) (addPieces) (dictionary st) (charToInt st) (center st) (usedTiles st) (boardFunc st)

    let remFromHand ms st = 
        let remPieces = List.fold (fun acc (c, (id, (ch, p))) -> MultiSet.removeSingle id acc) (hand st) ms 
        newState (playerNumber st) (remPieces) (dictionary st) (charToInt st) (center st) (usedTiles st) (boardFunc st)
    
    let addToBoard ms st = 
        let placedTiles = List.fold (fun (acc:Map<int*int, tile>) (c, (id, (ch, p))) -> acc.Add(c, (Set.empty.Add(ch, p)))) (usedTiles st) (ms) 
        newState (playerNumber st) (hand st) (dictionary st) (charToInt st) (center st) (placedTiles) (boardFunc st)
    
    let query st c = 
        match usedTiles st |> Map.tryFind(c) with
        | Some x -> UsedSquare(Map.find(fst(x.MaximumElement)) (charToInt st), fst(x.MaximumElement), snd(x.MaximumElement))
        | None -> 
            match c |> boardFunc st with
            | Some x -> UnusedSquare(x)
            | None -> Hole 