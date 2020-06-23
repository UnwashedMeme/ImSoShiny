module Splendor.Bank

type Count = int

// type count = int

// type Asset =
//     | Mine of Red | Green | Brown | White | Blue | Gold
//     | Coin of Red | Green | Brown | White | Blue | Gold


type Asset =
    | White
    | Blue
    | Green
    | Red
    | Brown
    | Gold

type Mine = Mine of Asset
type Coin = Coin of Asset

let ALL_COINS = [| Coin White; Coin Blue; Coin Green; Coin Red; Coin Brown; Coin Gold|]]

type Bank =
    { Red: Count
      Green: Count
      Brown: Count
      White: Count
      Blue: Count
      Gold: Count }
    member this.GetCoinCount =
        function
        | Coin Red -> this.Red
        | Coin Green -> this.Green
        | Coin Brown -> this.Brown
        | Coin White -> this.White
        | Coin Blue -> this.Blue
        | Coin Gold -> this.Gold
    /// Get the total count of all coins in the bank
    member this.InventoryCount =
        Seq.sumBy this.GetCoinCount ALL_COINS

let deposit (coin : Coin) (bank : Bank) =
    let count = (bank.GetCoinCount coin) + 1
    Ok (match coin with
             | Coin Red -> { bank with Red = count }
             | Coin Brown -> { bank with Brown = count }
             | Coin White -> { bank with White = count }
             | Coin Green -> { bank with Green = count }
             | Coin Blue -> { bank with Blue = count }
             | Coin Gold -> { bank with Gold = count })

let withdraw (coin: Coin) (bank: Bank) =
    let count = (bank.GetCoinCount coin) - 1
    if count >= 0 then
        Ok
            (match coin with
             | Coin Red -> { bank with Red = count }
             | Coin Brown -> { bank with Brown = count }
             | Coin White -> { bank with White = count }
             | Coin Green -> { bank with Green = count }
             | Coin Blue -> { bank with Blue = count }
             | Coin Gold -> { bank with Gold = count })
    else
        Result.Error "Not enough coins in bank"
