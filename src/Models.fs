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
        let getVP m :int = m.victoryPoints
        Seq.sum (Seq.concat [
                    (this.cards |> Seq.map (fun m -> m.victoryPoints));
                    (this.nobles|> Seq.map (fun b -> b.victoryPoints)); 
        ])
          
        
type GameState = {
    players: Player list;
    bank: Bank;
    tiers: Deck list;
    tiershow: Card list;
    nobles: Noble list;
}

//TODO: Split into separate Action Types per server state
type Action =     
    | Draw2OfSame of Coin
    | Draw3Different of Coin * Coin * Coin
    | BuyCard of Card
    | ReserveCard of Card
    | BuyNoble of Noble
    | DiscardCoins of Coin list
    | NOP

type PlayerAction = Player * Action