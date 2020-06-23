module Splendor.Gamestate


open Splendor.Bank
open Splendor.Card
open Splendor.Player


let (>>=) m f = Result.bind f m

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

type Turn =
    { Player: Player
      MainAction: MainAction
      DicardAction: DiscardCoinsAction
      Noble: BuyNobleAction
      Phase: TurnPhase }

type GameState =
    { Turns: Turn list
      Players: Player list
      ActivePlayerIdx: int

      TargetVictoryPoints: VPs
      Bank: Bank
      Tier1Cards: Deck
      Tier2Cards: Deck
      Tier3Cards: Deck
      Nobles: Noble list }

    member this.PlayerCount = Seq.length this.Players
    member this.EndOfRound = this.ActivePlayerIdx - 1 = this.PlayerCount

    member this.IsGameSetup: bool = this.Turns.Length = 0

    member this.IsFinalRound: bool =
        let isAWinner (p: Player) =
            p.VictoryPoints >= this.TargetVictoryPoints

        let players = this.Players
        players |> Seq.exists isAWinner

    member this.IsGameOver: bool = this.IsFinalRound && this.EndOfRound

    member this.ActivePlayer = Seq.item this.ActivePlayerIdx this.Players

    member this.NextPlayer =
        { this with
              ActivePlayerIdx = (this.ActivePlayerIdx + 1) % this.PlayerCount }


type Action =
    | MainAction of MainAction
    | DiscardCoinsAction of DiscardCoinsAction
    | BuyNobleAction of BuyNobleAction


module ProcessTurn =
    let operateOnCentralBank (fn: Bank -> Result<Bank, 'a>) (gamestate: GameState) =
        let bank = gamestate.Bank
        let nextstate nb = { gamestate with Bank = nb }
        bank |> fn |> Result.map nextstate

    let operateOnPlayer (fn: Player -> Result<Player, 'a>) (gamestate: GameState) =
        let player = gamestate.ActivePlayer

        let nextstate np =
            let replacer =
                fun p -> if player.Id = p.Id then np else p

            { gamestate with
                  Players = List.map replacer gamestate.Players }

        player |> fn |> Result.map nextstate

    let operateOnPlayerBank fn gamestate =
        let playerMod (player: Player) =
            let bank = player.Bank
            let newplayer npb = { player with Bank = npb }
            bank |> fn |> Result.map newplayer

        gamestate |> operateOnPlayer playerMod

    let draw2OfSame (coin: Coin) (gamestate: GameState) =
        let withdraw2 bank = bank |> withdraw coin >>= withdraw coin
        let deposit2 bank = bank |> deposit coin >>= deposit coin
        let hasEnoughCoinsRemaining bank =
            if bank.GetCoinCount coin >= 2 then
                Ok bank
            else
                Error "When taking 2 coins you must leave at least 2 coins of that color in the bank".
        gamestate
            |> operateOnCentralBank withdraw2
            >>= operateOnCentralBank hasEnoughCoinsRemaining
            >>= operateOnPlayerBank deposit2


    let draw3Different (coin1, coin2, coin3) (gamestate: GameState) =
        if coin1 = coin2 || coin2 = coin3 || coin1 = coin3 then
            Error "When drawing 3 coins they must be distinct"
        else
            let withdraw3 bank =
                bank
                |> withdraw coin1
                >>= withdraw coin2
                >>= withdraw coin3

            let deposit3 bank =
                bank
                |> deposit coin1
                >>= deposit coin2
                >>= deposit coin3

            gamestate
            |> operateOnCentralBank withdraw3
            >>= operateOnPlayerBank deposit3

    let buyCard card gamestate = Error "Can't afford it"

    let reserveCard card gamestate = Error "Already reserved three"


    let dispatchMainAction (turn: Turn) (gamestate: GameState) =
        match turn.MainAction with
        | MainAction.Draw2OfSame coin -> draw2OfSame coin gamestate
        | MainAction.Draw3Different (coin1, coin2, coin3) -> draw3Different (coin1, coin2, coin3) gamestate
        | MainAction.BuyCard card -> buyCard card gamestate
        | MainAction.ReserveCard card -> reserveCard card gamestate

    let discardCoins (turn: Turn) (gamestate: GameState) =
        match turn.DicardAction with
        | DiscardCoinsAction.NOP ->
            if gamestate.ActivePlayer.HasTooManyCoins
            then Error "Player has too many coins, must discard"
            else Ok gamestate
        | DiscardCoinsAction.Discard coins ->
            let withdrawAll (bank: Bank) =
                let withdraw1 (bankres: Result<Bank, string>) coin =
                    match bankres with
                    | Result.Ok bank -> withdraw coin bank
                    | Result.Error err -> Result.Error err

                Seq.fold withdraw1 (Ok bank) coins

            operateOnPlayerBank withdrawAll gamestate

    let buyNoble (turn: Turn) (gamestate: GameState) =
        match turn.Noble with
        | BuyNobleAction.NOP -> Ok gamestate
        | BuyNobleAction.Buy noble ->
            if Seq.contains noble gamestate.Nobles then Error "That noble is not available" else Ok gamestate

    let finish (turn: Turn) (gamestate: GameState) =
        if gamestate.IsGameOver then Ok gamestate else Ok gamestate.NextPlayer

    let processTurn turn gamestate =
        gamestate
        >>= dispatchMainAction turn
        >>= discardCoins turn
        >>= buyNoble turn
        >>= finish turn
