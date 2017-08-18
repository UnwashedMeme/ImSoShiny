module Splendor.Server
open Splendor.Models
open Splendor.Events

module MainActions =
    let draw2OfSame (gamestate:TurnData) (player:Player) (coin:Coin) =
        let withdrawer b = Withdraw b coin
        gamestate.bank 
            |> withdrawer 
            |> (Result.bind withdrawer)
            |> (Result.bind (fun b -> Ok { gamestate with bank = b }))

    let draw3Different (gamestate:TurnData) player (coin1,coin2,coin3) = 
        Error "Not different"

    let buyCard gamestate player card = 
        Error "Can't afford it"

    let reserveCard gamestate player card = 
        Error "Already reserved one"

    let dispatch gamestate player = function
        | MainAction.Draw2OfSame coin -> draw2OfSame gamestate player coin
        | MainAction.Draw3Different (coin1,coin2,coin3) -> draw3Different gamestate player (coin1,coin2,coin3)
        | MainAction.BuyCard card -> buyCard gamestate player card
        | MainAction.ReserveCard card -> reserveCard gamestate player card        

let discardCoins gamestate player discardCoinsAction = 
    Error "You don't have those coins."

let buyNoble gamestate player buyNobleAction =
    Error "You don't have the cards."

let nextPhase = function
    | Main -> DiscardCoins
    | DiscardCoins -> BuyNoble
    | BuyNoble -> Finish
    | Finish -> Main
let anyPlayerOverTargetVP td = false
let lastPlayerOfRound td = false

let nextPlayer td = 
    Turn td 

let endGame (td:TurnData) = 
    GameState.EndOfGame {
        players = td.players;
        targetVictoryPoints = td.targetVictoryPoints;
    }

let finishTurn (td:TurnData) = 
    // does (any player have > targetVictoryPoints
     //      and last player) -> end of game
     // next player
    if (anyPlayerOverTargetVP td && lastPlayerOfRound td) then
        endGame td
    else
        nextPlayer td

let validateCorrectPlayer fromPlayer gamestate =
    let playerUp = gamestate.currentPlayer
    if playerUp.id <> fromPlayer.id then
        Error "Not your turn"
    else
        Ok gamestate

let validatePhase action gamestate = 
    Error "Wrong phase"

let dispatchAction action turndata =
    let player = turndata.currentPlayer
    match action with 
    | Action.MainAction ma -> MainActions.dispatch turndata player ma 
    | Action.DiscardCoinsAction dca-> discardCoins turndata player dca
    | Action.BuyNobleAction bna -> buyNoble turndata player bna

let finishPhase turndata =
    Ok (match turndata.phase with
        | Finish -> finishTurn turndata
        | phase -> (Turn {turndata with phase = (nextPhase phase)}))
    
    

let (>>=) m f = Result.bind f m

let processTurnAction player (action:Action) turnData =
    turnData
    |> validateCorrectPlayer player 
    >>= validatePhase action 
    >>= dispatchAction action
    >>= finishPhase 