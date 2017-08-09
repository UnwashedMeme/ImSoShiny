module Splendor.Models

//open Fable.Core
//open Fable.Core.JsInterop
//open Fable.Import

type Url = string
type Qty = int

type Gem = Red | Green | Brown | White | Blue | Gold
type Coin = | Gem of Gem

/// count upward to victory
type IHasPoints =
    abstract VictoryPoints: int

type Card =
    {
        image: Url;
        cost: Gem list;
        provides: Gem;
        victoryPoints: int;
    }
    interface IHasPoints with
        member this.VictoryPoints = this.victoryPoints


type Noble =
    {
        image: Url;
        cost: Gem list;
        victoryPoints: int;
    }
    interface IHasPoints with
        member this.VictoryPoints = this.victoryPoints

type Deck = {
    cards: Card list
}

type Bank = {
    inventory: Coin list
}


type Player =
    {
        coins: Coin list;
        cards: Card list;
        nobles: Noble list;
    }
    interface IHasPoints with
        member this.VictoryPoints =
            let getVp (x:IHasPoints) = x.VictoryPoints
            [
                (this.cards |> Seq.map getVp);
                (this.nobles|> Seq.map getVp);
            ]
            |> Seq.concat
            |> Seq.sum


type GameState = {
    players: Player list;
    bank: Bank;
    tiers: Deck list;
    tiershow: Card list;
    nobles: Noble list;
}

//TODO: Split into separate Action Types per server state
type MainAction =
    | Draw2OfSame of Coin
    | Draw3Different of Coin * Coin * Coin
    | BuyCard of Card
    | ReserveCard of Card

type DiscardCoinsAction =
    | DiscardCoins of Coin list
    | NOP
type BuyNobleAction =
    | BuyNoble of Noble
    | NOP

type Action =
  | MainAction of MainAction
  | DiscardCoinsAction of DiscardCoinsAction
  | BuyNobleAction of BuyNobleAction
