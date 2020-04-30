namespace NutBot

open ScrabbleUtil
open MultiSet
open Eval

module ScrabbleState  = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, and your player numer but it could, potentially, 
    // keep track of other useful
    // information, such as number of players, player turn, etc.

    type HandState = {
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
    }

    type Board = {
        boardFunc : boardFun
        center : coord 
        usedTiles : Map<int * int, tile>
    }

    // type Direction = 
    //     // Up and Left are the wrong direction for words and only used when going in reverse with GADDAG
    //     | Right of int * int
    //     | Left of int * int
    //     | Down of int * int
    //     | Up of int * int


    let mkState pn h = { playerNumber = pn; hand = h }

    let newState pn hand = mkState pn hand
    
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand

    // let oppositeDirection = 
    //     function 
    //     | Right -> Left 
    //     | Left -> Right 
    //     | Down -> Up  
    //     | Up -> Down
    
    // let coordinates direction : Direction -> Direction
    // let horizontalDirection direction : Direction -> Direction
    // let verticalDirection direction : Direction -> Direction