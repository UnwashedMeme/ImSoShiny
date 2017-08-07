module Tests
open Expecto
open Splendor.Models
open Splendor.Events
open Splendor.Server


let emptyPlayer = { 
            coins = [];
            cards = [];
            nobles = [];
        }

let card1 = {
    image = "foo";
    cost = [Green; Blue; White];
    provides = Green; 
    victoryPoints = 0;
}
let card2 = {
    image = "bar";
    cost = [Red; Blue; White;];
    provides = Green; 
    victoryPoints = 2;

}
let noble1 = {
    image = "noble";
    cost = [Green; Blue; White; White;];
    victoryPoints = 3
}
[<Tests>]
let playerTests =
     testList "Player behavior" [
        test "Victory Points-0" {
            Expect.equal emptyPlayer.VictoryPoints 0 "An empty player has no victory Points";
        };
        test "Victory Points >0" {
            let player = {
                coins = []; nobles = [];
                cards = [card1; card2];
            }
            Expect.equal player.VictoryPoints 2 "A player should have victory points equal to their cards";
        };
        test "VictoryPoints nobles + cards" {
            let player = {
                coins = []; 
                nobles = [noble1];
                cards = [card1; card2]
            }
            Expect.equal player.VictoryPoints 5 "A player should have victory points equal to their cards & nobles";
        };
    ]

[<Tests>]
let updateGameStateTests =
    let player1:Player = {
        coins = []; 
        nobles = [noble1];
        cards = [card1; card2]
    }
    let player2 =  {
        coins = [Coin.Gem Green]; 
        nobles = [noble1];
        cards = [card1; card2]
    }
    let gameState:GameState = {
        players = [player1;];
        bank = { inventory = [] };
        tiers = [];
        tiershow = [];
        nobles = [];
    }
    let action = Draw2OfSame (Coin.Gem Green)
    testList "PlayerAction Validators" [
        testList "validateCorrectPlayer" [
            test "Right player" {
                Expect.equal (validateCorrectPlayer gameState (player1, action)) None "The player1 is up, player1 sent action"
            };
            test "Wrong player" {
                match (validateCorrectPlayer gameState (player2, action)) with
                    | None -> failwith "reported no errors for wrongplayer"
                    | Some (ServerEvent.InvalidAction (returnPlayer, returnAction, reason)) -> 
                        Expect.equal returnPlayer player2 "reported back to the wrong player"
                        Expect.equal returnAction action "reported back the wrong action" 
                    | Some (other) -> failwith (sprintf "Got %A" other)
            };
        ];
        testList "validate Draw2OfSame" [
            test "Not This Action" {
                Expect.isNone (validateDraw2OfSame gameState (player1, Draw3Different (Coin.Gem White, Coin.Gem Blue, Coin.Gem Green))) "Shouldn't be commenting on this action type"
            };
            test "Bank has None" {
                Expect.isSome (validateDraw2OfSame gameState (player1, Draw2OfSame (Coin.Gem White))) "There aren't two whites in bank"
            };
            test "Bank has one" {
                let bank = { inventory = [Coin.Gem White]}
                let gameState = { gameState with bank = bank }
                Expect.isSome (validateDraw2OfSame gameState (player1, Draw2OfSame (Coin.Gem White))) "There aren't two whites in bank"
            };
            test "Bank has enough" {
                let bank = { inventory = [Coin.Gem White; Coin.Gem White]}
                let gameState = { gameState with bank = bank }
                Expect.isNone (validateDraw2OfSame gameState (player1, Draw2OfSame (Coin.Gem White))) "There were enough coins in bank"
            };
        ];

        testList "validate Draw3Different" [
            test "Not This Action" {
                Expect.isNone (validateDraw3Different gameState (player1, Draw2OfSame (Coin.Gem White))) "Shouldn't be commenting on this action type"
            };
            test "Bank has None" {
                let draw3action = Draw3Different (Coin.Gem White, Coin.Gem Blue, Coin.Gem Green)
                Expect.isSome (validateDraw3Different gameState (player1, draw3action)) "There aren't enough coins in bank"
            };
            test "Bank has one" {
                let draw3action = Draw3Different (Coin.Gem White, Coin.Gem Blue, Coin.Gem Green)
                let bank = { inventory = [Coin.Gem White]}
                let gameState = { gameState with bank = bank }
                Expect.isSome (validateDraw3Different gameState (player1, draw3action)) "There aren't two whites in bank"
            };
            test "Bank has enough" {
                let draw3action = Draw3Different (Coin.Gem White, Coin.Gem Blue, Coin.Gem Green)
                let bank = { inventory = [Coin.Gem White; Coin.Gem White; Coin.Gem Blue; Coin.Gem Green]}
                let gameState = { gameState with bank = bank }
                Expect.isNone (validateDraw3Different gameState (player1, draw3action)) "There were enough coins in bank"
            };
        ];
        
    ]
