module Tests.Models

open Microsoft.FSharp.Reflection

open Expecto
open FsCheck

open Splendor.Models
open Splendor.Bank
open Tests.Init

let config = BaseConfig

[<Tests>]
let cardGenTests =
    testList "Test the card generator"
        [ testPropertyWithConfig config "Should have a reasonable number of victory points" (fun (card: Card) ->
              0 <= card.victoryPoints) ]

[<Tests>]
let BankTests =
    testList "bank withdraw"
        [ testPropertyWithConfig config "Should have one less or error"
          <| fun (bank: Bank) (c: Coin) ->
              let count = bank.GetCoinCount c
              match Withdraw c bank with
              | Ok bank -> (bank.GetCoinCount c) = (count - 1)
              | Error reason -> count = 0 ]



let emptyPlayer =
    { id = 0
      coins = []
      cards = []
      nobles = []
      reservedCards = [] }

[<Tests>]
let playerTests =
    testList "Player behavior"
        [ test "Victory Points-0" { Expect.equal emptyPlayer.VictoryPoints 0 "An empty player has no victory Points" }
          testPropertyWithConfig config "Has Victory Points"
          <| fun (player: Player) -> player.VictoryPoints >= 0

          testPropertyWithConfig config "Victory Points >0"
          <| fun (card: Card) ->
              let card1 = { card with victoryPoints = 2 }
              let card2 = { card with victoryPoints = 0 }

              let player =
                  { emptyPlayer with
                        cards = [ card1; card2 ] }

              Expect.equal player.VictoryPoints 2 "A player should have victory points equal to their cards"

          testPropertyWithConfig config "VictoryPoints nobles + cards"
          <| fun (card: Card) (noble: Noble) ->
              let card1 = { card with victoryPoints = 2 }
              let card2 = { card with victoryPoints = 4 }
              let noble1 = { noble with victoryPoints = 3 }

              let player =
                  { emptyPlayer with
                        nobles = [ noble1 ]
                        cards = [ card1; card2 ] }

              Expect.equal player.VictoryPoints 9 "A player should have victory points equal to their cards & nobles" ]

[<Tests>]
let turnDataTests =
    testList "TurnData generation"
        [ testPropertyWithConfig config "currentPlayer should be a game player"
          <| fun (turndata: TurnData) -> Expect.contains turndata.players turndata.currentPlayer ]


// module SimpleUnionCaseInfoReflection =

//   // will crash if 'T contains members which aren't only tags
//   let Construct<'T> (caseInfo: UnionCaseInfo) =
//      FSharpValue.MakeUnion(caseInfo, [||]) :?> 'T

//   let GetUnionCaseInfoAndInstance<'T> (caseInfo: UnionCaseInfo) =
//     (caseInfo, Construct<'T> caseInfo)

//   let AllCases<'T> =
//     FSharpType.GetUnionCases(typeof<'T>)
//     |> Seq.map Construct<'T>

//let coinlist = SimpleUnionCaseInfoReflection.AllCases<Coin>
let coinlist =
    [ Coin White
      Coin Blue
      Coin Green
      Coin Red
      Coin Brown
      Coin Gold ]

[<Tests>]
let bankTests =
    testList "Test bank properties"
        [ testPropertyWithConfig config "Withdraw should always be positive"
          <| fun (bank: Bank) (c: Coin) ->
              match Withdraw c bank with
              | Error e -> ()
              | Ok newBank ->
                  let check c = (bank.GetCoinCount c) >= 0
                  Expect.all coinlist check "No value should be less than 0" ]
