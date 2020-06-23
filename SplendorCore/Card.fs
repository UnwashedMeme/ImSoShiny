module Splendor.Card

open Splendor.Bank

type Url = string
type VPs = int

type Card =
    { Image: Url
      Cost: Asset list
      Provides: Mine
      VictoryPoints: VPs }

type Noble =
    { Image: Url
      Cost: Mine list
      VictoryPoints: VPs }

type Deck = { BackImage: Url; Cards: Card list }

/// Return a shuffled deck
let shuffle deck :Deck = deck // TODO: implement

/// Return the list of up to 4 cards that are at the top and currently displayed.
let getDisplayedCards deck = deck.Cards; //TODO: implement
