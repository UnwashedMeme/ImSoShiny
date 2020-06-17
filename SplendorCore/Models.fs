module Splendor.Models

open Splendor.Bank

type Url = string
type Qty = int
type VPs = int


type Card =
    { image: Url
      cost: Gem list
      provides: Mine
      victoryPoints: VPs }

type Noble =
    { image: Url
      cost: Mine list
      victoryPoints: VPs }



type Player =
    { id: int
      coins: Coin list
      cards: Card list
      nobles: Noble list
      reservedCards: Card list }
    member this.VictoryPoints: VPs =
        [ (this.cards |> Seq.map (fun m -> m.victoryPoints))
          (this.nobles |> Seq.map (fun b -> b.victoryPoints)) ]
        |> Seq.concat
        |> Seq.sum


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


type SetupData =
    { players: Player list
      targetVictoryPoints: VPs }



type TurnData =
    { players: Player list
      targetVictoryPoints: VPs
      currentPlayer: Player
      phase: TurnPhase
      bank: Bank
      tier1: Card list
      tier2: Card list
      tier3: Card list
      nobles: Noble list }

type EndOfGameData =
    { players: Player list
      targetVictoryPoints: VPs }


type Turn =
    { Player: Player
      MainAction: MainAction
      DicardAction: DiscardCoinsAction
      Noble: BuyNobleAction }

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



type GameState =
    | Setup of SetupData
    | Turn of TurnData
    | EndOfGame of EndOfGameData

type Action =
    | MainAction of MainAction
    | DiscardCoinsAction of DiscardCoinsAction
    | BuyNobleAction of BuyNobleAction
