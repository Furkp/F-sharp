namespace NutBot

open ScrabbleLib

open ScrabbleServer
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.Net.Sockets
open System.IO
open DebugPrint

open Eval
open Parser
open FParsec

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, and your player numer but it could, potentially, 
    // keep track of other useful
    // information, such as number of players, player turn, etc.

    type HandState = {
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
    }

    type BoardState = {
        center : coord 
        defaultSquare : squareFun
        squares       : boardFun
        placedSquare : Map<int,squareFun>
        boardFunc : coord -> Map<int, squareFun> 
    }

    }

    let mkState pn h = { playerNumber = pn; hand = h }

    let newState pn hand = mkState pn hand
    
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand

module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Thread.Sleep(5000) // only here to not confuse the pretty-printer. Remove later.
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            let input =  System.Console.ReadLine()
            let move = RegEx.parseMove input

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) (SMPass)) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) msg) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPassed i) -> 
                (* your idiot of a enemy passed*)
                debugPrint "Enemy passed" 
                debugPrint (sprintf "id: %d" i)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                debugPrint "CMPlaySuccess"
                debugPrint (sprintf "ms: %A" ms)
                debugPrint (sprintf "points: %d" points)
                debugPrint (sprintf "newPieces: %A" newPieces)
                let remPieces = List.fold (fun acc (c, (id, (ch, p))) -> MultiSet.removeSingle id acc) st.hand ms
                let addPieces = List.fold (fun acc (id, c) -> MultiSet.add id c acc) remPieces newPieces
                let st' = State.mkState st.playerNumber addPieces  // This state needs to be updated
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                debugPrint "CMPlayed"
                debugPrint (sprintf "pid: %d" pid)
                debugPrint (sprintf "ms: %A" ms)
                debugPrint (sprintf "points: %d" points)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                debugPrint "CMPlayFailed"
                debugPrint (sprintf "pid: %d" pid)
                debugPrint (sprintf "ms: %A" ms)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

    let startGame 
            (boardP : boardProg) 
            (alphabet : string) 
            (words : string list) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)
        // debugPrint (boardP.prog)

        let stmParser = ImpParser.runTextParser ImpParser.stmParse  
        let stm = stmParser boardP.prog
        
        printfn "stm"
        printfn "%A" stm
        // debugPrint (sprintf "stm: %A" stm)
        printfn "/stm"

        debugPrint (sprintf "/stm")
        // let sqs = boardP.squares
        // let a (b:squareProg) = 4
        // let map = Map.map (fun k v -> Map.map (fun k2 v2 -> 
        //     debugPrint(v2) 
        //     stmntToBoardFun (stmParser v2)) v) sqs
        // let boardFun = stmntToBoardFun 
        // debugPrint (stm)

        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.newState playerNumber handSet )
        