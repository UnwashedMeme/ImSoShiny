module Splendor.Models

//open Fable.Core
//open Fable.Core.JsInterop
//open Fable.Import

type Url = string
type Qty = int

type Gem = Red | Green | Brown | White | Blue | Gold
type Coin = Gem of Gem

type Card = {
    image: Url;
    cost: Gem list;
    provides: Gem; 
    victoryPoints: int;
}

type Noble = {
    image: Url;
    cost: Gem list;
}

type Deck = {
    cards: Card list
}

type Bank = {
    inventory: Coin list
}


type Player = {
    coins: Coin list;
    mines: Card list;
    nobles: Noble list;
} with
    member this.VictoryPoints :int = 0
        //Array.sum this.mines (fun m -> m.victoryPoints) +   Array.sum this.nobles (fun n -> n.victoryPoints)

type GameState = {
    players: Player list;
    bank: Bank;
    tier1: Deck;
    tier1show: Card list;
    tier2: Deck;
    tier2show: Card list;
    tier3: Deck;
    tier3show: Card list;
    nobles: Noble list;
}

type Action =     
    | Draw2OfSame of Coin
    | Draw3Different of Coin * Coin * Coin
    | BuyCard of Card
    | ReserveCard of Card
    | BuyNoble of Noble
