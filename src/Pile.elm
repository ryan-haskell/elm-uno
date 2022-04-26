module Pile exposing
    ( Pile, empty
    , addCardToTop, removeBottomCards
    , topCard
    , view
    )

{-|

@docs Pile, empty
@docs addCardToTop, removeBottomCards
@docs topCard
@docs view

-}

import Card exposing (Card)
import Html exposing (Html)
import Html.Attributes


type Pile
    = Pile (List Card)


empty : Pile
empty =
    Pile []


addCardToTop : Card -> Pile -> Pile
addCardToTop card (Pile pile) =
    Pile (card :: pile)


removeBottomCards : Pile -> ( List Card, Pile )
removeBottomCards (Pile pile) =
    ( List.drop 1 pile
    , Pile (List.take 1 pile)
    )


topCard : Pile -> Maybe Card
topCard (Pile pile) =
    List.head pile


view : Pile -> Html msg
view pile =
    Html.div [ Html.Attributes.class "pile", Html.Attributes.id "pile" ]
        [ case topCard pile of
            Nothing ->
                Card.viewEmptyPile

            Just topCard_ ->
                Card.view topCard_
        ]
