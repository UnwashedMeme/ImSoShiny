module Tests.Models

open Expecto
open FsCheck

open Splendor.Models
open Tests.Init

[<Tests>]
let cardGenTests =
     testList "Test the card generator" [
        testPropertyWithConfig config "Should have a reasonable number of victory points"  
            (fun (card:Card) -> 0 <= card.victoryPoints)
    ]

[<Tests>]
let BankTests = 
    testList "bank withdraw" [
        testPropertyWithConfig config "Should have one less or error" 
            <| fun (bank:Bank) (c:Coin) ->
                let count = bank.GetCoinCount c
                match Withdraw bank c with
                    | Ok bank -> (bank.GetCoinCount c) = (count - 1)
                    | Error reason -> count = 0;
    ]
                    
    

let emptyPlayer = { 
    coins = [];
    cards = [];
    nobles = [];
    reservedCards = [];
}

[<Tests>]
let playerTests =
     testList "Player behavior" [
        test "Victory Points-0" {
            Expect.equal emptyPlayer.VictoryPoints 0 "An empty player has no victory Points";
        };
        testPropertyWithConfig config "Has Victory Points" 
            <| fun (player:Player) ->
                player.VictoryPoints >= 0;
           
        testPropertyWithConfig config "Victory Points >0" 
            <| fun (card:Card)->
                let card1 = {card with victoryPoints = 2}
                let card2 = {card with victoryPoints = 0}
                let player = {emptyPlayer with cards = [card1; card2] }
                Expect.equal player.VictoryPoints 2 
                    "A player should have victory points equal to their cards";
        
        testPropertyWithConfig config "VictoryPoints nobles + cards" 
            <| fun (card:Card) (noble:Noble) ->
                let card1 = {card with victoryPoints = 2}
                let card2 = {card with victoryPoints = 4}
                let noble1 = {noble with victoryPoints = 3}
                let player = {emptyPlayer with
                                nobles = [noble1];
                                cards = [card1; card2]
                }
                Expect.equal player.VictoryPoints 9
                    "A player should have victory points equal to their cards & nobles";      ;
    ]

[<Tests>]
let turnDataTests =
    testList "TurnData generation" [
        testPropertyWithConfig config "currentPlayer should be a game player"
            <| fun (turndata:TurnData) -> 
                turndata.players |> Seq.contains turndata.currentPlayer;
        testPropertyWithConfig config "player count should be < 5"                
            <| fun (turndata) ->
                (Seq.length turndata.players) < 6
    ]
        
