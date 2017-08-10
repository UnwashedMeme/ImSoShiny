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
        testProperty "Has Victory Points" <| fun (player:Player) ->
           player.VictoryPoints >= 0
           
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
    let action = MainAction (Draw2OfSame (Coin.Gem Green))
    testList "PlayerAction Validators" [
        testList "validateCorrectPlayer" [
            test "Right player" {
                Expect.equal (validateCorrectPlayer gameState player1 action) None "The player1 is up, player1 sent action"
            };
            test "Wrong player" {
                Expect.isSome (validateCorrectPlayer gameState player2 action) "On the wrong player it should say so"
            };
        ];
        
    ]
