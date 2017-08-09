module Splendor.Models

//open Fable.Core
//open Fable.Core.JsInterop
//open Fable.Import

type Url = string
type Qty = int

type Gem = Red | Green | Brown | White | Blue | Gold
type Coin = | Gem of Gem

type Card = {
    image: Url;
    cost: Gem list;
    provides: Gem;
    victoryPoints: int;
}

type Noble = {
    image: Url;
    cost: Gem list;
    victoryPoints: int;
}

type Deck = {
    cards: Card list
}

type Bank = {
    inventory: Coin list
}


type Player = {
    coins: Coin list;
    cards: Card list;
    nobles: Noble list;
} with
    member this.VictoryPoints :int =
        [
            (this.cards |> Seq.sumBy (fun x -> x.victoryPoints));
            (this.nobles |> Seq.sumBy (fun x -> x.victoryPoints));
        ] |> Seq.sum


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
