module Splendor.Player

open Splendor.Bank
open Splendor.Card

type Player =
    { Id: int
      Bank: Bank
      Cards: Card list
      Nobles: Noble list
      ReservedCards: Card list }
    member this.VictoryPoints: VPs =
        [ (this.Cards |> Seq.map (fun m -> m.VictoryPoints))
          (this.Nobles |> Seq.map (fun b -> b.VictoryPoints)) ]
        |> Seq.concat
        |> Seq.sum

    member this.HasTooManyCoins =
      this.Bank.InventoryCount > 10
