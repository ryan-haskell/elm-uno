module Main exposing (main)

import Browser
import Card exposing (Card)
import Deck exposing (Deck)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Random


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- INIT


type alias Model =
    { phase : Phase
    , deck : Deck
    , pile : List Card
    , hand : List Card
    , counter : Int
    }


type Phase
    = ReadyToPlay
    | PlayingGame
    | PlayerWonTheGame


init : Model
init =
    { phase = ReadyToPlay
    , deck = Deck.new (Random.initialSeed 0)
    , pile = []
    , hand = []
    , counter = 1
    }



-- UPDATE


type Msg
    = PlayerClickedDeck
    | PlayerClickedCardInHand Int Card
    | PlayerClickedPlayAgain


update : Msg -> Model -> Model
update msg model =
    case msg of
        PlayerClickedDeck ->
            case model.phase of
                ReadyToPlay ->
                    startNewGame model

                PlayingGame ->
                    drawAnotherCard model

                PlayerWonTheGame ->
                    startNewGame model

        PlayerClickedCardInHand index card ->
            case List.head model.pile of
                Nothing ->
                    model

                Just topCardOnPile ->
                    if Card.isOkayToPlay { topCardOnPile = topCardOnPile, cardFromHand = card } then
                        playCardOntoPile index card model
                            |> checkIfGameOver

                    else
                        model

        PlayerClickedPlayAgain ->
            startNewGame model


startNewGame : Model -> Model
startNewGame model =
    let
        newDeck : Deck
        newDeck =
            Deck.new (Random.initialSeed model.counter)

        updated1 : { cards : List Card, deck : Deck }
        updated1 =
            Deck.draw 7 newDeck

        updated2 : { cards : List Card, deck : Deck }
        updated2 =
            Deck.draw 1 updated1.deck

        maybeCardToAddToPile : Maybe Card
        maybeCardToAddToPile =
            List.head updated2.cards
    in
    { model
        | phase = PlayingGame
        , counter = model.counter + 1
        , deck = updated2.deck
        , hand = updated1.cards
        , pile =
            case maybeCardToAddToPile of
                Just card ->
                    [ card ] ++ model.pile

                Nothing ->
                    model.pile
    }


drawAnotherCard : Model -> Model
drawAnotherCard model =
    let
        updated : { cards : List Card, deck : Deck }
        updated =
            Deck.draw 1 model.deck

        maybeCard : Maybe Card
        maybeCard =
            List.head updated.cards
    in
    { model
        | deck = updated.deck
        , hand =
            case maybeCard of
                Just card ->
                    model.hand ++ [ card ]

                Nothing ->
                    model.hand
    }


playCardOntoPile : Int -> Card -> Model -> Model
playCardOntoPile index card model =
    let
        removeAtIndex : Int -> List a -> List a
        removeAtIndex i list =
            List.take i list ++ List.drop (i + 1) list
    in
    { model
        | hand = removeAtIndex index model.hand
        , pile = card :: model.pile
    }


checkIfGameOver : Model -> Model
checkIfGameOver model =
    if List.isEmpty model.hand then
        { model | phase = PlayerWonTheGame }

    else
        model



-- VIEW


view : Model -> Html Msg
view model =
    Html.div [ Html.Attributes.class "game" ]
        [ viewGitHubLink
        , viewPlayArea model
        , viewPlayerHand model
        , viewYouWonMessage model
        ]


viewGitHubLink : Html Msg
viewGitHubLink =
    Html.a
        [ Html.Attributes.class "github-link"
        , Html.Attributes.target "_blank"
        , Html.Attributes.rel "noopener noreferrer"
        , Html.Attributes.href "https://github.com/ryannhg/elm-uno"
        , Html.Attributes.attribute "aria-label" "View code on GitHub"
        ]
        []


viewPlayArea : Model -> Html Msg
viewPlayArea model =
    Html.div [ Html.Attributes.class "play-area" ]
        [ viewPile model
        , viewDeck model
        ]


viewPile : Model -> Html Msg
viewPile model =
    case model.pile of
        [] ->
            Card.viewEmptyPile

        topCard :: _ ->
            Card.view topCard


viewDeck : Model -> Html Msg
viewDeck model =
    if Deck.isEmpty model.deck then
        Card.viewEmptyDeck

    else
        Html.button
            [ Html.Events.onClick PlayerClickedDeck ]
            [ Card.viewBackOfCard ]


viewPlayerHand : Model -> Html Msg
viewPlayerHand model =
    Html.div
        [ Html.Attributes.class "hand"
        , Html.Attributes.classList
            [ ( "hand--has-cards", not (List.isEmpty model.hand) )
            ]
        ]
        (List.indexedMap viewCardInHand model.hand)


viewCardInHand : Int -> Card -> Html Msg
viewCardInHand index card =
    Html.button
        [ Html.Attributes.class "hand__card-button"
        , Html.Events.onClick (PlayerClickedCardInHand index card)
        ]
        [ Card.view card ]


viewYouWonMessage : Model -> Html Msg
viewYouWonMessage model =
    case model.phase of
        PlayerWonTheGame ->
            Html.div [ Html.Attributes.class "dialog" ]
                [ Html.p [ Html.Attributes.class "dialog__message" ] [ Html.text "You won!!" ]
                , Html.button
                    [ Html.Events.onClick PlayerClickedPlayAgain
                    , Html.Attributes.class "button"
                    ]
                    [ Html.text "Play again!" ]
                ]

        _ ->
            Html.text ""
