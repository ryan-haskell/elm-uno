module Main exposing (main)

import Browser
import Card exposing (Card)
import Deck exposing (Deck)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Keyed
import Random
import Random.List
import Time


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
    , direction : Direction
    , currentPlayerId : Int
    , playersHand : List Card
    , computerHands : Dict PlayerId (List Card)
    , declaredColor : Maybe Card.Color
    , seed : Int
    }


type Direction
    = Clockwise
    | CounterClockwise


reverseDirection : Direction -> Direction
reverseDirection direction =
    case direction of
        Clockwise ->
            CounterClockwise

        CounterClockwise ->
            Clockwise


type alias PlayerId =
    Int


type Phase
    = ReadyToPlay
    | PlayingGame
    | DeclaringWildCardColor
    | PlayerWonTheGame Int


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { phase = ReadyToPlay
      , deck = Deck.new (Random.initialSeed 0)
      , pile = []
      , currentPlayerId = 0
      , direction = Clockwise
      , playersHand = []
      , computerHands =
            Dict.fromList
                [ ( 1, [] )
                , ( 2, [] )
                , ( 3, [] )
                ]
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
    | ComputerTakesTurn


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayerClickedDeck ->
            case model.phase of
                ReadyToPlay ->
                    ( startNewGame model, Cmd.none )

                PlayingGame ->
                    if isHumanPlayersTurn model then
                        ( drawAnotherCardIntoHand model
                            |> checkIfDeckIsEmpty
                            |> moveOnToNextPlayer Nothing
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                DeclaringWildCardColor ->
                    ( model, Cmd.none )

                PlayerWonTheGame _ ->
                    ( model, Cmd.none )

        PlayerClickedCardInHand card ->
            case List.head model.pile of
                Nothing ->
                    ( model, Cmd.none )

                Just topCardOnPile ->
                    if
                        isHumanPlayersTurn model
                            && Card.isOkayToPlay
                                { declaredColor = model.declaredColor
                                , topCardOnPile = topCardOnPile
                                , cardFromHand = card
                                }
                    then
                        ( playCardOntoPile card model
                            |> checkIfGameOver
                            |> checkIfPlayedWildCard card
                            |> checkIfDeckIsEmpty
                            |> moveOnToNextPlayer (Just card)
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

        ComputerTakesTurn ->
            case model.phase of
                ReadyToPlay ->
                    ( model, Cmd.none )

                PlayingGame ->
                    ( haveComputerTakeTurn model
                        |> checkIfGameOver
                        |> checkIfDeckIsEmpty
                    , Cmd.none
                    )

                DeclaringWildCardColor ->
                    ( model, Cmd.none )

                PlayerWonTheGame _ ->
                    ( model, Cmd.none )


isHumanPlayersTurn : Model -> Bool
isHumanPlayersTurn model =
    0 == model.currentPlayerId


isComputersTurn : Model -> Bool
isComputersTurn model =
    model.currentPlayerId > 0


subscriptions : Model -> Sub Msg
subscriptions model =
    if isComputersTurn model then
        Time.every 1000 (\_ -> ComputerTakesTurn)

    else
        Sub.none


startNewGame : Model -> Model
startNewGame model =
    let
        newDeck : Deck
        newDeck =
            Deck.new (Random.initialSeed model.seed)

        afterDrawPlayerHand : { cards : List Card, deck : Deck }
        afterDrawPlayerHand =
            Deck.draw 7 newDeck

        computerPlayerCount : Int
        computerPlayerCount =
            Dict.size model.computerHands

        afterDrawingToComputers :
            { deck : Deck
            , computerHands : Dict PlayerId (List Card)
            }
        afterDrawingToComputers =
            Dict.foldl loop
                { deck = afterDrawPlayerHand.deck
                , computerHands = model.computerHands
                }
                model.computerHands

        loop :
            PlayerId
            -> List Card
            ->
                { deck : Deck
                , computerHands : Dict PlayerId (List Card)
                }
            ->
                { deck : Deck
                , computerHands : Dict PlayerId (List Card)
                }
        loop playerId currentHand { deck, computerHands } =
            let
                afterDrawing =
                    Deck.draw 7 deck
            in
            { computerHands = Dict.insert playerId afterDrawing.cards computerHands
            , deck = afterDrawing.deck
            }

        afterDrawingPileCard : { cards : List Card, deck : Deck }
        afterDrawingPileCard =
            Deck.draw 1 afterDrawingToComputers.deck

        maybeCardToAddToPile : Maybe Card
        maybeCardToAddToPile =
            List.head afterDrawingPileCard.cards

        shouldDrawAnotherCard : Bool
        shouldDrawAnotherCard =
            case maybeCardToAddToPile of
                Just card ->
                    Card.isOneOfTheWildCards card

                Nothing ->
                    False

        updatedModel : Model
        updatedModel =
            { model
                | phase = PlayingGame
                , currentPlayerId = 0
                , seed = model.seed + 1
                , deck = afterDrawingPileCard.deck
                , playersHand = afterDrawPlayerHand.cards
                , computerHands = afterDrawingToComputers.computerHands
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
                    Card.isOneOfTheWildCards card

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
        , playersHand =
            case maybeCard of
                Just card ->
                    model.playersHand ++ [ card ]

                Nothing ->
                    model.playersHand
    }


playCardOntoPile : Card -> Model -> Model
playCardOntoPile card model =
    { model
        | playersHand = List.filter (doesNotHaveMatchingId card) model.playersHand
        , pile = card :: model.pile
    }


doesNotHaveMatchingId : Card -> Card -> Bool
doesNotHaveMatchingId card1 card2 =
    Card.toUniqueId card1 /= Card.toUniqueId card2


haveComputerTakeTurn : Model -> Model
haveComputerTakeTurn model =
    let
        findPlayableCardsInHand : Card -> List Card -> List Card
        findPlayableCardsInHand topCardOnPile entireHand =
            entireHand
                |> List.filter
                    (\card ->
                        Card.isOkayToPlay
                            { declaredColor = model.declaredColor
                            , topCardOnPile = topCardOnPile
                            , cardFromHand = card
                            }
                    )
    in
    case ( List.head model.pile, Dict.get model.currentPlayerId model.computerHands ) of
        ( Just topCardOnPile, Just hand ) ->
            let
                playableCards : List Card
                playableCards =
                    findPlayableCardsInHand topCardOnPile hand

                ( wildCards, nonWildCards ) =
                    List.partition Card.isOneOfTheWildCards
                        playableCards

                randomlySelectedNonWildCard : Maybe Card
                randomlySelectedNonWildCard =
                    Random.initialSeed model.seed
                        |> Random.step (Random.List.choose nonWildCards)
                        |> Tuple.first
                        |> Tuple.first

                randomlySelectedWildCard : Maybe Card
                randomlySelectedWildCard =
                    Random.initialSeed model.seed
                        |> Random.step (Random.List.choose wildCards)
                        |> Tuple.first
                        |> Tuple.first

                cardToPlay : Maybe Card
                cardToPlay =
                    case ( randomlySelectedNonWildCard, randomlySelectedWildCard ) of
                        ( Just nonWildCard, _ ) ->
                            Just nonWildCard

                        ( _, Just wildCard ) ->
                            Just wildCard

                        _ ->
                            Nothing

                removeCardFromComputersHand : Card -> Dict PlayerId (List Card)
                removeCardFromComputersHand card =
                    Dict.update model.currentPlayerId
                        (\maybeComputersHand ->
                            case maybeComputersHand of
                                Just computersHand ->
                                    Just (List.filter (doesNotHaveMatchingId card) computersHand)

                                Nothing ->
                                    Nothing
                        )
                        model.computerHands

                addCardsToComputersHand : List Card -> Dict PlayerId (List Card)
                addCardsToComputersHand newCards =
                    Dict.update model.currentPlayerId
                        (\maybeComputersHand ->
                            case maybeComputersHand of
                                Just computersHand ->
                                    Just (computersHand ++ newCards)

                                Nothing ->
                                    Nothing
                        )
                        model.computerHands

                computerDeclaredWild : Card -> Maybe Card.Color
                computerDeclaredWild card =
                    if Card.isOneOfTheWildCards card then
                        Random.initialSeed model.seed
                            |> Random.step (Random.List.choose [ Card.Red, Card.Yellow, Card.Green, Card.Blue ])
                            |> Tuple.first
                            |> Tuple.first

                    else
                        Nothing
            in
            case cardToPlay of
                Just card ->
                    { model
                        | seed = model.seed + 1
                        , computerHands = removeCardFromComputersHand card
                        , pile = card :: model.pile
                        , declaredColor = computerDeclaredWild card
                    }
                        |> moveOnToNextPlayer (Just card)

                Nothing ->
                    let
                        afterComputerDraws =
                            Deck.draw 1 model.deck
                    in
                    { model
                        | seed = model.seed + 1
                        , computerHands = addCardsToComputersHand afterComputerDraws.cards
                        , deck = afterComputerDraws.deck
                    }
                        |> moveOnToNextPlayer Nothing

        _ ->
            model


checkIfGameOver : Model -> Model
checkIfGameOver model =
    if List.isEmpty model.playersHand then
        { model | phase = PlayerWonTheGame 0 }

    else
        case computerIdWithoutCards model of
            Just computerId ->
                { model | phase = PlayerWonTheGame computerId }

            Nothing ->
                model


computerIdWithoutCards : Model -> Maybe PlayerId
computerIdWithoutCards model =
    Dict.filter
        (\_ hand -> List.isEmpty hand)
        model.computerHands
        |> Dict.keys
        |> List.head


checkIfPlayedWildCard : Card -> Model -> Model
checkIfPlayedWildCard card model =
    case model.phase of
        PlayingGame ->
            if Card.isOneOfTheWildCards card then
                { model | phase = DeclaringWildCardColor }

            else
                { model | declaredColor = Nothing }

        _ ->
            { model | declaredColor = Nothing }


checkIfDeckIsEmpty : Model -> Model
checkIfDeckIsEmpty model =
    if Deck.isEmpty model.deck then
        let
            newDeck : Deck
            newDeck =
                Deck.reshuffle
                    (Random.initialSeed model.seed)
                    (List.drop 1 model.pile)
        in
        { model
            | deck = newDeck
            , seed = model.seed + 1
            , pile = List.take 1 model.pile
        }

    else
        model


moveOnToNextPlayer : Maybe Card -> Model -> Model
moveOnToNextPlayer maybeCardPlayed model =
    let
        totalPlayerCount : Int
        totalPlayerCount =
            1 + Dict.size model.computerHands
    in
    case maybeCardPlayed of
        Just cardPlayed ->
            if Card.isReverse cardPlayed then
                { model
                    | currentPlayerId =
                        getNextPlayerId
                            { distance = 1
                            , direction = reverseDirection model.direction
                            , total = totalPlayerCount
                            , currentPlayerId = model.currentPlayerId
                            }
                    , direction = reverseDirection model.direction
                }

            else if Card.isSkip cardPlayed || Card.isDrawTwo cardPlayed || Card.isWildDraw4 cardPlayed then
                { model
                    | currentPlayerId =
                        getNextPlayerId
                            { distance = 2
                            , direction = model.direction
                            , total = totalPlayerCount
                            , currentPlayerId = model.currentPlayerId
                            }
                }

            else
                { model
                    | currentPlayerId =
                        getNextPlayerId
                            { distance = 1
                            , direction = model.direction
                            , total = totalPlayerCount
                            , currentPlayerId = model.currentPlayerId
                            }
                }

        Nothing ->
            { model
                | currentPlayerId =
                    getNextPlayerId
                        { distance = 1
                        , direction = model.direction
                        , total = totalPlayerCount
                        , currentPlayerId = model.currentPlayerId
                        }
            }


getNextPlayerId : { distance : Int, direction : Direction, total : Int, currentPlayerId : PlayerId } -> PlayerId
getNextPlayerId options =
    case options.direction of
        Clockwise ->
            (options.currentPlayerId + options.distance)
                |> modBy options.total

        CounterClockwise ->
            (options.currentPlayerId - options.distance)
                |> modBy options.total



-- VIEW


view : Model -> Html Msg
view model =
    Html.div [ Html.Attributes.class "game" ]
        [ viewGitHubLink
        , viewPlayArea model
        , viewComputerHands model
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


viewComputerHands : Model -> Html Msg
viewComputerHands model =
    let
        viewComputerHand : Int -> Side -> Html Msg
        viewComputerHand id side =
            case Dict.get id model.computerHands of
                Just hand ->
                    viewHandOnSide
                        { side = side
                        , hand = hand
                        }

                Nothing ->
                    Html.text ""
    in
    Html.div [ Html.Attributes.class "computer-hands" ]
        [ viewComputerHand 1 Left
        , viewComputerHand 2 Top
        , viewComputerHand 3 Right
        ]


viewPlayerHand : Model -> Html Msg
viewPlayerHand model =
    viewHandOnSide
        { side = Bottom
        , hand = model.playersHand
        }


type Side
    = Top
    | Left
    | Right
    | Bottom


viewHandOnSide : { side : Side, hand : List Card } -> Html Msg
viewHandOnSide options =
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
            [ ( "hand--has-cards", not (List.isEmpty options.hand) )
            , ( "hand--top", options.side == Top )
            , ( "hand--left", options.side == Left )
            , ( "hand--right", options.side == Right )
            , ( "hand--bottom", options.side == Bottom )
            ]
        ]
        (List.map toKeyedNodeTuple options.hand)


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

        PlayerWonTheGame 0 ->
            viewYouWonDialog model

        PlayerWonTheGame id ->
            viewYouLostDialog id model

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


viewYouLostDialog : PlayerId -> Model -> Html Msg
viewYouLostDialog idOfPlayerWhoWon model =
    viewDialogWithContent
        [ Html.p [ Html.Attributes.class "dialog__message" ]
            [ Html.text
                ("Computer {{id}} won..."
                    |> String.replace "{{id}}" (String.fromInt idOfPlayerWhoWon)
                )
            ]
        , Html.button
            [ Html.Events.onClick PlayerClickedPlayAgain
            , Html.Attributes.class "button"
            ]
            [ Html.text "Play again?" ]
        ]


viewDialogWithContent : List (Html msg) -> Html msg
viewDialogWithContent content =
    Html.div [ Html.Attributes.class "dialog" ]
        [ Html.div [ Html.Attributes.class "dialog__background" ] []
        , Html.div [ Html.Attributes.class "dialog__content" ] content
        ]
