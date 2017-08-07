module Splendor.Server
open Splendor.Models
open Splendor.Events

type ServerState = 
    | SettingUpGame
    | WaitingForPlayerAction
    | WaitingForCoinDiscard
    | WaitingForNobleBuy
    | FinishingTurn
    | EndGame


let validateCorrectPlayer (gameState:GameState) (playerAction:PlayerAction) =
    let playerUp = List.head gameState.players
    let (fromPlayer, action) = playerAction
    if playerUp <> fromPlayer then
        Some (ServerEvent.InvalidAction(fromPlayer, action, "Not your turn"))
    else
        None

let validateDraw2OfSame (gameState:GameState) (playerAction:PlayerAction) : ServerEvent option = 
    None

let validateDraw3Different (gameState:GameState) (playerAction:PlayerAction) : ServerEvent option = 
    None

let validateBuyCard (gameState:GameState) (playerAction:PlayerAction) : ServerEvent option = 
    None
    
let validateReserveCard (gameState:GameState) (playerAction:PlayerAction) : ServerEvent option = 
    None

let validateBuyNoble (gameState:GameState) (playerAction:PlayerAction) : ServerEvent option = 
    None

let updateGameState (gameState:GameState) (playerAction:PlayerAction) =
        ServerEvent.GameStateUpdate gameState
        
       