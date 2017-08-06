module Splendor.Events
open Splendor.Models

type Reason = string

type ServerEvent = 
    | ItIsYourTurn of Player
    | YouWon of Player
    | GameStateUpdate of GameState
    | InvalidAction of Player * Action * Reason
    | Ping
    

type ClientCommand =
    | Action of Action
    | RequestGameState
    | Ping

