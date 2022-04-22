module Main exposing (main)

import Browser
import Card exposing (Card)
import Deck exposing (Deck)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Keyed
import Random


type alias Flags =
    { seed : Int
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { phase : Phase
    , deck : Deck
    , pile : List Card
    , hand : List Card
    , declaredColor : Maybe Card.Color
    , seed : Int
    }


type Phase
    = ReadyToPlay
    | PlayingGame
    | DeclaringWildCardColor
    | PlayerWonTheGame


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { phase = ReadyToPlay
      , deck = Deck.new (Random.initialSeed 0)
      , pile = []
      , hand = []
      , declaredColor = Nothing
      , seed = flags.seed
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = PlayerClickedDeck
    | PlayerClickedCardInHand Card
    | PlayerClickedPlayAgain
    | PlayerDeclaredColor Card.Color


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayerClickedDeck ->
            case model.phase of
                ReadyToPlay ->
                    ( startNewGame model, Cmd.none )

                PlayingGame ->
                    ( drawAnotherCardIntoHand model, Cmd.none )

                DeclaringWildCardColor ->
                    ( model, Cmd.none )

                PlayerWonTheGame ->
                    ( model, Cmd.none )

        PlayerClickedCardInHand card ->
            case List.head model.pile of
                Nothing ->
                    ( model, Cmd.none )

                Just topCardOnPile ->
                    if
                        Card.isOkayToPlay
                            { declaredColor = model.declaredColor
                            , topCardOnPile = topCardOnPile
                            , cardFromHand = card
                            }
                    then
                        ( playCardOntoPile card model
                            |> checkIfGameOver
                            |> checkIfPlayedWildCard card
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

        PlayerDeclaredColor color ->
            ( { model
                | declaredColor = Just color
                , phase = PlayingGame
              }
            , Cmd.none
            )

        PlayerClickedPlayAgain ->
            ( startNewGame model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


startNewGame : Model -> Model
startNewGame model =
    let
        newDeck : Deck
        newDeck =
            Deck.new (Random.initialSeed model.seed)

        afterDraw7 : { cards : List Card, deck : Deck }
        afterDraw7 =
            Deck.draw 7 newDeck

        afterDraw8 : { cards : List Card, deck : Deck }
        afterDraw8 =
            Deck.draw 1 afterDraw7.deck

        maybeCardToAddToPile : Maybe Card
        maybeCardToAddToPile =
            List.head afterDraw8.cards

        shouldDrawAnotherCard : Bool
        shouldDrawAnotherCard =
            case maybeCardToAddToPile of
                Just card ->
                    Card.isWild card

                Nothing ->
                    False

        updatedModel : Model
        updatedModel =
            { model
                | phase = PlayingGame
                , seed = model.seed + 1
                , deck = afterDraw8.deck
                , hand = afterDraw7.cards
                , pile =
                    case maybeCardToAddToPile of
                        Just card ->
                            [ card ]

                        Nothing ->
                            []
            }
    in
    if shouldDrawAnotherCard then
        drawAnotherCardOntoPile updatedModel

    else
        updatedModel


drawAnotherCardOntoPile : Model -> Model
drawAnotherCardOntoPile model =
    let
        updated : { cards : List Card, deck : Deck }
        updated =
            Deck.draw 1 model.deck

        maybeCard : Maybe Card
        maybeCard =
            List.head updated.cards

        shouldDrawAnotherCard : Bool
        shouldDrawAnotherCard =
            case maybeCard of
                Just card ->
                    Card.isWild card

                Nothing ->
                    False

        updatedModel : Model
        updatedModel =
            { model
                | deck = updated.deck
                , pile =
                    case maybeCard of
                        Just card ->
                            card :: model.pile

                        Nothing ->
                            model.pile
            }
    in
    if shouldDrawAnotherCard then
        drawAnotherCardOntoPile updatedModel

    else
        updatedModel


drawAnotherCardIntoHand : Model -> Model
drawAnotherCardIntoHand model =
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


playCardOntoPile : Card -> Model -> Model
playCardOntoPile card model =
    let
        doesNotHaveMatchingId : Card -> Bool
        doesNotHaveMatchingId cardInHand =
            Card.toUniqueId card /= Card.toUniqueId cardInHand
    in
    { model
        | hand = List.filter doesNotHaveMatchingId model.hand
        , pile = card :: model.pile
    }


checkIfGameOver : Model -> Model
checkIfGameOver model =
    if List.isEmpty model.hand then
        { model | phase = PlayerWonTheGame }

    else
        model


checkIfPlayedWildCard : Card -> Model -> Model
checkIfPlayedWildCard card model =
    case model.phase of
        PlayingGame ->
            if Card.isWild card then
                { model | phase = DeclaringWildCardColor }

            else
                { model | declaredColor = Nothing }

        _ ->
            { model | declaredColor = Nothing }



-- VIEW


view : Model -> Html Msg
view model =
    Html.div [ Html.Attributes.class "game" ]
        [ viewGitHubLink
        , viewPlayArea model
        , viewPlayerHand model
        , viewDialog model
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
        , viewDeclaredColor model
        ]


viewDeclaredColor : Model -> Html Msg
viewDeclaredColor model =
    case model.declaredColor of
        Just color ->
            Html.div [ Html.Attributes.class "declared-color" ] [ Html.text (Card.colorToName color) ]

        Nothing ->
            Html.text ""


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
    let
        toKeyedNodeTuple : Card -> ( String, Html Msg )
        toKeyedNodeTuple card =
            ( Card.toUniqueId card
            , viewCardInHand card
            )
    in
    Html.Keyed.node "div"
        [ Html.Attributes.class "hand"
        , Html.Attributes.classList
            [ ( "hand--has-cards", not (List.isEmpty model.hand) )
            ]
        ]
        (List.map toKeyedNodeTuple model.hand)


viewCardInHand : Card -> Html Msg
viewCardInHand card =
    Html.button
        [ Html.Attributes.class "hand__card-button"
        , Html.Events.onClick (PlayerClickedCardInHand card)
        ]
        [ Card.view card ]


viewDialog : Model -> Html Msg
viewDialog model =
    case model.phase of
        DeclaringWildCardColor ->
            viewDeclareColorDialog model

        PlayerWonTheGame ->
            viewYouWonDialog model

        _ ->
            Html.text ""


viewDeclareColorDialog : Model -> Html Msg
viewDeclareColorDialog model =
    let
        viewChooseColorButton color =
            Html.button
                [ Html.Events.onClick (PlayerDeclaredColor color)
                , Html.Attributes.class "button"
                ]
                [ Html.text (Card.colorToName color) ]
    in
    viewDialogWithContent
        [ Html.p [ Html.Attributes.class "dialog__message" ] [ Html.text "Pick a color:" ]
        , Html.div [ Html.Attributes.class "dialog__buttons" ]
            (List.map viewChooseColorButton
                [ Card.Red
                , Card.Yellow
                , Card.Green
                , Card.Blue
                ]
            )
        ]


viewYouWonDialog : Model -> Html Msg
viewYouWonDialog model =
    viewDialogWithContent
        [ Html.p [ Html.Attributes.class "dialog__message" ] [ Html.text "You won!" ]
        , Html.button
            [ Html.Events.onClick PlayerClickedPlayAgain
            , Html.Attributes.class "button"
            ]
            [ Html.text "Play again!" ]
        ]


viewDialogWithContent : List (Html msg) -> Html msg
viewDialogWithContent content =
    Html.div [ Html.Attributes.class "dialog" ]
        [ Html.div [ Html.Attributes.class "dialog__background" ] []
        , Html.div [ Html.Attributes.class "dialog__content" ] content
        ]
