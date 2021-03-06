module Tests.Actions

open Expecto
open FsCheck
open Splendor.Bank
open Splendor.Gamestate
open Splendor.Actions

open Tests.Init

let config = BaseConfig

[<Tests>]
let testMainActions =
    testList "Main Actions"
        [ testPropertyWithConfig config "draw2OfSame withdraw or error"
          <| fun (turndata: TurnData) (coin: Coin) ->

              match MainActions.draw2OfSame turndata turndata.currentPlayer coin with
              | Error e -> ()
              | Ok ntd ->
                  let player = ntd.currentPlayer
                  Expect.equal (ntd.bank.GetCoinCount coin) ((turndata.bank.GetCoinCount coin) - 2)
                      "bank should have 2 less of that coin"
                  Expect.equal (Seq.length player.coins) ((Seq.length turndata.currentPlayer.coins) + 2)
                      "Player should have 2 more coins"

          testPropertyWithConfig config "draw2OfSame player list"
          <| fun (td: TurnData) ->
              let player = td.currentPlayer
              let getPlayerIds players = players |> List.map (fun p -> p.id)
              Expect.contains (getPlayerIds td.players) player.id "At the start the player should be in the list"


              let originalPlayer = td.currentPlayer
              match MainActions.draw2OfSame td player (Coin Green) with
              | Error e -> ()
              | Ok newTurnData ->
                  Expect.contains td.players td.currentPlayer "Missing the current player"
                  Expect.equal (getPlayerIds newTurnData.players) (getPlayerIds td.players) "Players changed" ]


[<Tests>]
let testDispatchAction =
    testList "Dispatch Action"
        [ testPropertyWithConfig config "next phase"
          <| fun (action: Action) (td: TurnData) ->
              match dispatchAction action td with
              | Error e -> ()
              | Ok nextTurnData ->
                  Expect.equal nextTurnData.phase (nextPhase td.phase) "Should have given the next phase" ]

let checkCorrectNextPlayer (players : Player list) ( player1 : Player ) (player2: Player) =
    Expect.equal (nextPlayer players player1) player2 "Incorrect next player"


let checkMainAction (gs : GameState) (turn : Turn) ( ngs : GameState ) =
    Expect.equal true false "TODO: Check for legal actions"
    Expect.equal true false "TODO: Check any purchase was from first 4 cards"
    Expect.equal true false "TODO: check bank was updated"

let checkBuyNoble (gs : GameState) (turn : Turn) ( ngs : GameState ) =
    Expect.equal true false "TODO: check noble got removed from list"

[<Tests>]
let testDispatchAction2 =
    testList "Dispatch Action2"
        [ testPropertyWithConfig config "next phase"
          <| fun (turn: Turn) (gs2: ActiveGameState) ->
              match takeTurn turn gs2 with
              | Error e -> ()
              | Ok ngs2 ->
                  match ngs2.IsGameOver with
                  | true -> ()
                  | false ->
                    checkCorrectNextPlayer gs2.Players gs2.ActivePlayer ngs2.ActivePlayer
                    checkMainAction gs2 turn ngs2
                    checkBuyNoble gs2 turn ngs2

        ]
