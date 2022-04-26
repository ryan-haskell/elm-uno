module Deck exposing
    ( Deck, new
    , draw
    , isEmpty, size
    , reshuffle
    )

{-|

@docs Deck, new
@docs deal, draw
@docs isEmpty, size
@docs reshuffle

-}

import Card exposing (Card)
import Random
import Random.List


type Deck
    = Deck { cards : List Card }


new : Random.Seed -> Deck
new seed =
    Deck
        { cards = shuffle seed Card.unshuffledDeck
        }


isEmpty : Deck -> Bool
isEmpty (Deck { cards }) =
    List.isEmpty cards


size : Deck -> Int
size (Deck { cards }) =
    List.length cards


reshuffle : Random.Seed -> List Card -> Deck
reshuffle seed cards =
    Deck
        { cards = shuffle seed cards
        }


draw : Int -> Deck -> { cards : List Card, deck : Deck }
draw numberOfCards (Deck { cards }) =
    -- TODO: Fail if couldn't draw enough cards
    { cards = List.take numberOfCards cards
    , deck = Deck { cards = List.drop numberOfCards cards }
    }


shuffle : Random.Seed -> List Card -> List Card
shuffle seed deck =
    let
        ( shuffledDeck, _ ) =
            Random.step (Random.List.shuffle deck) seed
    in
    shuffledDeck
