module Splendor.Card

open Splendor.Bank

type Url = string
type VPs = int

type Card =
    { Image: Url
      Cost: Gem list
      Provides: Mine
      VictoryPoints: VPs }

type Noble =
    { Image: Url
      Cost: Mine list
      VictoryPoints: VPs }

type Deck = { BackImage: Url; Cards: Card list }
