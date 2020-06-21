module Splendor.Actions

open Splendor.Bank
open Splendor.Gamestate
open Splendor.Card
open Splendor.Player
open Splendor.Events

let (>>=) m f = Result.bind f m

let operateOnPlayer fn turndata =
    let currentPlayer = turndata.CurrentPlayer

    let replacer np =
        fun p -> if currentPlayer.Id = p.Id then np else p

    let nextTurnData newplayer =
        { turndata with
              CurrentPlayer = newplayer
              Players = turndata.Players |> List.map (replacer newplayer) }

    currentPlayer |> fn |> Result.map nextTurnData


let operateOnBank fn turndata =
    let nextTurnData bank = { turndata with Bank = bank }
    fn turndata.Bank |> Result.map nextTurnData


module MainActions =
    let draw2OfSame (turndata: TurnData) (player: Player) (coin: Coin) =
        let withdraw2 bank = bank |> Withdraw coin >>= Withdraw coin

        let addToPlayer player =
            { player with
                  Coins = coin :: coin :: player.Coins }

        turndata
        |> operateOnPlayer (addToPlayer >> Ok)
        >>= operateOnBank withdraw2

    let draw3Different (gamestate: TurnData) player (coin1, coin2, coin3) =
        if coin1 = coin2 || coin2 = coin3 || coin1 = coin3 then
            Error "Not different"
        else
            let withdraw3 bank =
                bank
                |> Withdraw coin1
                >>= Withdraw coin2
                >>= Withdraw coin3

            gamestate |> operateOnBank withdraw3

    let buyCard gamestate player card = Error "Can't afford it"

    let reserveCard gamestate player card = Error "Already reserved one"

    let dispatch gamestate player =
        function
        | MainAction.Draw2OfSame coin -> draw2OfSame gamestate player coin
        | MainAction.Draw3Different (coin1, coin2, coin3) -> draw3Different gamestate player (coin1, coin2, coin3)
        | MainAction.BuyCard card -> buyCard gamestate player card
        | MainAction.ReserveCard card -> reserveCard gamestate player card

let discardCoins gamestate player discardCoinsAction = Error "You don't have those coins."

let buyNoble gamestate player buyNobleAction = Error "You don't have the cards."

let nextPhase =
    function
    | Main -> DiscardCoins
    | DiscardCoins -> BuyNoble
    | BuyNoble -> Finish
    | Finish -> Main

let anyPlayerOverTargetVP td = false
let lastPlayerOfRound td = false

let nextPlayer (players : Player list) ( player: Player) =
    // TODO: implement
    player


let endGame (td: TurnData) =
    GameState.EndOfGame
        { players = td.players
          targetVictoryPoints = td.targetVictoryPoints }

let finishTurn (td: TurnData) =
    // does (any player have > targetVictoryPoints
    //      and last player) -> end of game
    // next player
    if (anyPlayerOverTargetVP td && lastPlayerOfRound td)
    then endGame td
    else Turn(td)

let validateCorrectPlayer fromPlayer gamestate =
    let playerUp = gamestate.CurrentPlayer
    if playerUp.Id <> fromPlayer.Id then Error "Not your turn" else Ok gamestate

let validatePhase action gamestate = Error "Wrong phase"

let dispatchAction action turndata =
    let player = turndata.CurrentPlayer
    match action with
    | Action.MainAction ma -> MainActions.dispatch turndata player ma
    | Action.DiscardCoinsAction dca -> discardCoins turndata player dca
    | Action.BuyNobleAction bna -> buyNoble turndata player bna

let finalizeCheck turndata =
    Ok
        (match turndata.phase with
         | Finish -> finishTurn turndata
         | phase ->
             (Turn
                 { turndata with
                       phase = (nextPhase phase) }))



let processTurnAction player (action: Action) turnData =
    turnData
    |> validateCorrectPlayer player
    >>= validatePhase action
    >>= dispatchAction action
    >>= finalizeCheck

let takeTurn (turn : Turn) ( state: GameState ) = Ok state





let (foo x y z = x + y + z
