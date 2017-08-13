module Splendor.Server
open Splendor.Models
open Splendor.Events

let validateCorrectPlayer fromPlayer gamestate =
    let playerUp = gamestate.currentPlayer
    if playerUp <> fromPlayer then
        Error "Not your turn"
    else
        Ok gamestate

let validatePhase action gamestate = 
    Error "Wrong phase"

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

let discardCoins gamestate player discardCoinsAction = 
    Error "You don't have those coins."

let buyNoble gamestate player buyNobleAction =
    Error "You don't have the cards."
    

let mainAction gamestate player = function
    | MainAction.Draw2OfSame coin -> draw2OfSame gamestate player coin
    | MainAction.Draw3Different (coin1,coin2,coin3) -> draw3Different gamestate player (coin1,coin2,coin3)
    | MainAction.BuyCard card -> buyCard gamestate player card
    | MainAction.ReserveCard card -> reserveCard gamestate player card


let nextPhase = function
    | Main -> DiscardCoins
    | DiscardCoins -> BuyNoble
    | BuyNoble -> Finish
    | Finish -> Main

let doAction gamestate player = function 
    | Action.MainAction ma -> mainAction gamestate player ma 
    | Action.DiscardCoinsAction dca-> discardCoins gamestate player dca
    | Action.BuyNobleAction bna -> buyNoble gamestate player bna

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

let finishPhase turndata =
    Ok (match turndata.phase with
        | Finish -> finishTurn turndata
        | phase -> (Turn {turndata with phase = (nextPhase phase)}))
    
    

let (>>=) m f = Result.bind f m

let receiveCommand (gamestate:GameState) player (action:Action): ServerEvent =
    gamestate
    |> function  
        | Turn x -> Ok x
        | _ -> Error "Wrong stage of game"
    >>= validatePhase action 
    >>= validateCorrectPlayer player 
    >>= fun turndata -> doAction turndata player action
    >>= finishPhase 
    |> function 
        | Ok gamestate -> ServerEvent.GameStateUpdate gamestate
        | Error err ->  ServerEvent.InvalidAction (player, action, err)
