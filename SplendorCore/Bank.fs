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

let ASSETS =
    [| White
       Blue
       Green
       Red
       Brown
       Gold |]

type Bank =
    { Red: Count
      Green: Count
      Brown: Count
      White: Count
      Blue: Count
      Gold: Count }
    member this.GetCount =
        function
        | White -> this.White
        | Blue -> this.Blue
        | Green -> this.Green
        | Red -> this.Red
        | Brown -> this.Brown
        | Gold -> this.Gold

    member this.GetCoinCount(Coin asset) = this.GetCount asset
    /// Get the total count of all coins in the bank
    member this.InventoryCount = Seq.sumBy this.GetCount ASSETS

let deposit asset count (bank: Bank) =
    let count = (bank.GetCount asset) + count
    Ok
        (match asset with
         | Red -> { bank with Red = count }
         | Brown -> { bank with Brown = count }
         | White -> { bank with White = count }
         | Green -> { bank with Green = count }
         | Blue -> { bank with Blue = count }
         | Gold -> { bank with Gold = count })

let withdraw asset count (bank: Bank) =
    let count = (bank.GetCount asset) - count
    if count >= 0 then
        Ok
            (match asset with
             | Red -> { bank with Red = count }
             | Brown -> { bank with Brown = count }
             | White -> { bank with White = count }
             | Green -> { bank with Green = count }
             | Blue -> { bank with Blue = count }
             | Gold -> { bank with Gold = count })
    else
        Result.Error "Not enough coins in bank"
