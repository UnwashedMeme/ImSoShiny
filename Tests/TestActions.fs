module Tests.Actions
open Expecto
open FsCheck
open Splendor.Models
open Splendor.Actions

type ServerGen() =
    static member Player() : Arbitrary<VPs> = 
        Gen.choose (0,6) |> Arb.fromGen

let config = { FsCheckConfig.defaultConfig with arbitrary = [typeof<ServerGen>] }


[<Tests>]
let testMainActions =
     testList "Main Actions" [
        testPropertyWithConfig config "Draw 2 should withdraw or error" 
            <| fun (td:TurnData) (player:Player) (coin:Coin) ->
                match MainActions.draw2OfSame td player coin with
                    | Error e -> ()
                    | Ok newTd -> 
                        let bank = newTd.bank
                        let player = newTd.currentPlayer
                        Expect.equal (bank.GetCoinCount coin) ((td.bank.GetCoinCount coin) - 2) "bank should have 2 less of that coin"
                        Expect.equal (Seq.length player.coins) ((Seq.length td.currentPlayer.coins) + 2) "Player should have 2 more coins"
                    
     ]