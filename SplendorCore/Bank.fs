module Splendor.Bank

type Count = int

// type count = int

// type Gem =
//     | Mine of Red | Green | Brown | White | Blue | Gold
//     | Coin of Red | Green | Brown | White | Blue | Gold


type Gem =
    | Red
    | Green
    | Brown
    | White
    | Blue
    | Gold

type Mine = Mine of Gem
type Coin = Coin of Gem
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


let Withdraw (coin: Coin) (bank: Bank) =
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
