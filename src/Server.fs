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

let validateCorrectPlayer (gamestate:GameState) fromPlayer (action:Action) =
    let playerUp = List.head gamestate.players
    if playerUp <> fromPlayer then
        Some (ServerEvent.InvalidAction(fromPlayer, action, "Not your turn"))
    else
        None

let validatePhase gamestate player action : ServerEvent option = 
    Some (ServerEvent.InvalidAction (player, action, "Wrong phase"))


let draw2OfSame (gamestate:GameState) (player:Player) (coin:Coin) =
    ServerEvent.GameStateUpdate gamestate

let draw3Different (gamestate:GameState) player (coin1,coin2,coin3) = 
    ServerEvent.GameStateUpdate gamestate

let buyCard gamestate player card = 
    ServerEvent.GameStateUpdate gamestate
let reserveCard gamestate player card = 
    ServerEvent.GameStateUpdate gamestate

let mainAction gamestate player mainaction : ServerEvent = 
    match mainaction with
        | MainAction.Draw2OfSame coin -> draw2OfSame gamestate player coin
        | MainAction.Draw3Different (coin1,coin2,coin3) -> draw3Different gamestate player (coin1,coin2,coin3)
        | MainAction.BuyCard card -> buyCard gamestate player card
        | MainAction.ReserveCard card -> reserveCard gamestate player card


let receiveCommand (gamestate:GameState) player (action:Action): ServerEvent =
    match (validateCorrectPlayer gamestate player action) with 
        | Some se -> se
        | None -> (match action with
                    | Action.MainAction mainaction 
                        -> validatePhase gamestate player action
                           mainAction gamestate player mainaction 
                )
