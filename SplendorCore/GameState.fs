module Splendor.Gamestate

open Splendor.Bank
open Splendor.Card
open Splendor.Player

type Qty = int

//// Actions

type MainAction =
    | Draw2OfSame of Coin
    | Draw3Different of Coin * Coin * Coin
    | BuyCard of Card
    | ReserveCard of Card

type DiscardCoinsAction =
    | Discard of Coin list
    | NOP

type BuyNobleAction =
    | Buy of Noble
    | NOP

//// Gamestate data
type TurnPhase =
    | Main
    | DiscardCoins
    | BuyNoble
    | Finish

type TurnData =
    { Players: Player list
      TargetVictoryPoints: VPs
      CurrentPlayer: Player
      Phase: TurnPhase
      Bank: Bank
      Tier1: Card list
      Tier2: Card list
      Tier3: Card list
      Nobles: Noble list }

type EndOfGameData =
    { Players: Player list
      TargetVictoryPoints: VPs }


type Turn =
    { Player: Player
      MainAction: MainAction
      DicardAction: DiscardCoinsAction
      Noble: BuyNobleAction
      Phase: TurnPhase }

type GameState2 =
    { Turns: Turn list
      Players: Player list
      ActivePlayer: Player

      TargetVictoryPoints: VPs
      Bank: Bank
      Tier1Cards: Card list
      Tier2Cards: Card list
      Tier3Cards: Card list
      Nobles: Noble list }
    member this.IsGameOver: bool = false;


type Action =
    | MainAction of MainAction
    | DiscardCoinsAction of DiscardCoinsAction
    | BuyNobleAction of BuyNobleAction


let processTurn gamestate turn = gamestate
