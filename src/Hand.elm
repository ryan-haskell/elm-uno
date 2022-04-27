module Hand exposing
    ( Hand, empty, fromListOfCards
    , addCards, removeCard
    , isEmpty, size
    , findPlayableCards, lastCard
    , getAvailableColors
    , Side(..), view
    )

{-|

@docs Hand, empty, fromListOfCards
@docs addCards, removeCard
@docs isEmpty, size
@docs findPlayableCards, lastCard
@docs getAvailableColors
@docs Side, view

-}

import Card exposing (Card)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Keyed


type Hand
    = Hand (List Card)


empty : Hand
empty =
    Hand []


fromListOfCards : List Card -> Hand
fromListOfCards cards =
    Hand cards


addCards : List Card -> Hand -> Hand
addCards newCards (Hand existingCards) =
    Hand (existingCards ++ newCards)


removeCard : Card -> Hand -> Hand
removeCard cardToRemove (Hand cards) =
    let
        doesNotMatchId : Card -> Bool
        doesNotMatchId cardInHand =
            Card.toUniqueId cardInHand /= Card.toUniqueId cardToRemove
    in
    Hand (List.filter doesNotMatchId cards)


size : Hand -> Int
size (Hand cards) =
    List.length cards


lastCard : Hand -> Maybe Card
lastCard (Hand cards) =
    cards
        |> List.reverse
        |> List.head


isEmpty : Hand -> Bool
isEmpty hand =
    size hand == 0


type Side
    = Top
    | Left
    | Right
    | Bottom


view :
    { cardToHide : Maybe Card
    , side : Side
    , hand : Hand
    , isCurrentlyPlaying : Bool
    , shouldHideCards : Bool
    , onClick : Card -> msg
    }
    -> Html msg
view options =
    let
        (Hand cards) =
            options.hand

        toKeyedNodeTuple : Card -> ( String, Html msg )
        toKeyedNodeTuple card =
            ( Card.toUniqueId card
            , viewCardInHand card
            )

        viewCardInHand : Card -> Html msg
        viewCardInHand card =
            let
                shouldHideCard : Bool
                shouldHideCard =
                    case options.cardToHide of
                        Just cardToHide ->
                            Card.toUniqueId cardToHide == Card.toUniqueId card

                        Nothing ->
                            False
            in
            Html.button
                [ Html.Attributes.class "hand__card-button"
                , Html.Events.onClick (options.onClick card)
                , if shouldHideCard then
                    Html.Attributes.style "opacity" "0"

                  else
                    Html.Attributes.style "" ""
                ]
                [ if options.shouldHideCards then
                    Card.viewBackOfCard card

                  else
                    Card.view card
                ]
    in
    Html.Keyed.node "div"
        [ Html.Attributes.class "hand"
        , Html.Attributes.classList
            [ ( "hand--has-cards", not (isEmpty options.hand) )
            , ( "hand--active", options.isCurrentlyPlaying )
            , ( "hand--top", options.side == Top )
            , ( "hand--left", options.side == Left )
            , ( "hand--right", options.side == Right )
            , ( "hand--bottom", options.side == Bottom )
            ]
        ]
        (List.map toKeyedNodeTuple cards)


findPlayableCards : Maybe Card.Color -> Card -> Hand -> List Card
findPlayableCards declaredColor topCardOnPile (Hand cards) =
    cards
        |> List.filter
            (\card ->
                Card.isOkayToPlay
                    { declaredColor = declaredColor
                    , topCardOnPile = topCardOnPile
                    , cardFromHand = card
                    }
            )


getAvailableColors : Hand -> List Card.Color
getAvailableColors (Hand cards) =
    cards
        |> Card.getColorsForCards
