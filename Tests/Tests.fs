module Tests
open Expecto
open Splendor.Models

[<Tests>]
let tests =
     testList "Player behavior" [
        testCase "Victory Points-0" <| fun () ->
            let player: Player = { 
                coins = [];
                mines = [];
                nobles = [];
            }
            let foo = None
            Expect.equal player.VictoryPoints 0 "An empty player has no victory Points"
     ]
