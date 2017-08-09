module Splendor.Server
open Splendor.Models
open Splendor.Events

type ServerState = 
    | SettingUpGame
    | WaitingForMainAction
    | WaitingForCoinDiscard
    | WaitingForNobleBuy
    | FinishingTurn
    | EndGame

let validateCorrectPlayer (gamestate:GameState) fromPlayer (action:Action) : string option  =
    let playerUp = List.head gamestate.players
    if playerUp <> fromPlayer then
        Some "Not your turn"
    else
        None

let validatePhase gamestate player action  = 
    Some "Wrong phase"

let bindValidator validator gamestate player action =
    function 
        | Some err ->  Some err
        | None -> validator gamestate player action
    
let draw2OfSame (gamestate:GameState) (player:Player) (coin:Coin) =
    ServerEvent.GameStateUpdate gamestate

let draw3Different (gamestate:GameState) player (coin1,coin2,coin3) = 
    ServerEvent.GameStateUpdate gamestate

let buyCard gamestate player card = 
    ServerEvent.GameStateUpdate gamestate
let reserveCard gamestate player card = 
    ServerEvent.GameStateUpdate gamestate

let discardCoins gamestate player discardCoinsAction = 
    ServerEvent.GameStateUpdate gamestate

let buyNoble gamestate player buyNobleAction =
    ServerEvent.GameStateUpdate gamestate
    

let mainAction gamestate player mainaction : ServerEvent = 
    match mainaction with
        | MainAction.Draw2OfSame coin -> draw2OfSame gamestate player coin
        | MainAction.Draw3Different (coin1,coin2,coin3) -> draw3Different gamestate player (coin1,coin2,coin3)
        | MainAction.BuyCard card -> buyCard gamestate player card
        | MainAction.ReserveCard card -> reserveCard gamestate player card


let receiveCommand (gamestate:GameState) player (action:Action): ServerEvent =
    let validators = [validateCorrectPlayer; validatePhase]
    
    match validators |> Seq.tryPick (fun v-> v gamestate player action) with 
        | Some reason -> ServerEvent.InvalidAction (player,action,reason)
        | None -> match action with
                    | Action.MainAction ma -> mainAction gamestate player ma 
                    | Action.DiscardCoinsAction dca-> discardCoins gamestate player dca
                    | Action.BuyNobleAction bna -> buyNoble gamestate player bna
                
