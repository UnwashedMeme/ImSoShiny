module Splendor.Models

type Url = string
type Qty = int
type VPs = int

type Gem = Red | Green | Brown | White | Blue | Gold
type Mine = | Mine of Gem
type Coin = | Coin of Gem
// type Gem =
//     | Mine of Red | Green | Brown | White | Blue | Gold
//     | Coin of Red | Green | Brown | White | Blue | Gold


type Card = {
    image: Url;
    cost: Gem list;
    provides: Mine; 
    victoryPoints: VPs;
}

type Noble = {
    image: Url;
    cost: Mine list;
    victoryPoints: VPs;
}

type BankCount = int
type Bank = {
    red: BankCount;
    green: BankCount;
    brown: BankCount;
    white: BankCount;
    blue: BankCount;
    gold: BankCount;
} with
    member this.GetCoinCount = function
        | Coin Red -> this.red
        | Coin Green -> this.green
        | Coin Brown -> this.brown
        | Coin White -> this.white
        | Coin Blue -> this.blue
        | Coin Gold -> this.gold

let Withdraw (bank:Bank) (coin:Coin) =
        let count = (bank.GetCoinCount coin) - 1
        if count >= 0 then
            Ok (match coin with
                | Coin Red -> {bank with red = count}
                | Coin Brown -> {bank with brown = count}
                | Coin White -> {bank with white = count}
                | Coin Green -> {bank with green = count}
                | Coin Blue -> {bank with blue = count}
                | Coin Gold -> {bank with gold = count})
        else
            Result.Error "Not enough coins in bank"
        
 
type Player = {
    coins: Coin list;
    cards: Card list;
    nobles: Noble list;
    reservedCards: Card list;
} with
    member this.VictoryPoints : VPs = 
        [
            (this.cards  |> Seq.map (fun m -> m.victoryPoints)); 
            (this.nobles |> Seq.map (fun b -> b.victoryPoints)); 
        ] |> Seq.concat |> Seq.sum



//// Gamestate data

type TurnPhase = Main | DiscardCoins | BuyNoble | Finish

type SetupData = {
    players: Player list;
    targetVictoryPoints: VPs;
}

type TurnData = {
    players: Player list;
    targetVictoryPoints: VPs;
    currentPlayer: Player;
    phase: TurnPhase;
    bank: Bank;
    tier1: Card list;
    tier2: Card list;
    tier3: Card list;
    nobles: Noble list;
}
type EndOfGameData = {
    players: Player list;
    targetVictoryPoints: VPs;
}

type GameState = 
    | Setup of SetupData
    | Turn of TurnData
    | EndOfGame of EndOfGameData


type MainAction =     
    | Draw2OfSame of Coin
    | Draw3Different of Coin * Coin * Coin
    | BuyCard of Card
    | ReserveCard of Card

type DiscardCoinsAction = Coin list
type BuyNobleAction = Noble option

type Action =
  | MainAction of MainAction
  | DiscardCoinsAction of DiscardCoinsAction
  | BuyNobleAction of BuyNobleAction
