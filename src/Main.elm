module Main exposing (Model, checkIfDraw2OrDraw4, init, main)

import Browser
import Browser.Dom
import Card exposing (Card)
import Deck exposing (Deck)
import Dict exposing (Dict)
import Hand exposing (Hand)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Pile exposing (Pile)
import Platform exposing (Task)
import Process
import Random
import Random.List
import Task
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
    , pile : Pile
    , direction : Direction
    , currentPlayerId : Int
    , playersHand : Hand
    , computerHands : Dict PlayerId Hand
    , declaredColor : Maybe Card.Color
    , floatingCardState : FloatingCardState
    , seed : Int
    }


type FloatingCardState
    = NoFloatingCard
    | FloatingCardAtSource FloatingCard
    | FloatingCardAtDestination FloatingCard


type alias FloatingCard =
    { card : Card
    , source : BoundingRect
    , destination : BoundingRect
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
      , pile = Pile.empty
      , currentPlayerId = 0
      , direction = Clockwise
      , playersHand = Hand.empty
      , computerHands =
            Dict.fromList
                [ ( 1, Hand.empty )
                , ( 2, Hand.empty )
                , ( 3, Hand.empty )
                ]
      , declaredColor = Nothing
      , floatingCardState = NoFloatingCard
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
      -- Player hand to pile animation
    | AnimationHandToPileStep1
        { card : Card
        , result :
            Result
                Browser.Dom.Error
                { cardElement : BoundingRect
                , pileElement : BoundingRect
                }
        }
    | AnimationHandToPileStep2 FloatingCard
    | AnimationHandToPileComplete Card
      -- Deck to hand animation
    | AnimationDeckToHandStep1
        { card : Card
        , result :
            Result
                Browser.Dom.Error
                { cardElement : BoundingRect
                , deckElement : BoundingRect
                }
        }
    | AnimationDeckToHandStep2 FloatingCard
    | AnimationDeckToHandComplete


type alias BoundingRect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlayerClickedDeck ->
            case model.phase of
                ReadyToPlay ->
                    ( startNewGame model, Cmd.none )

                PlayingGame ->
                    if isHumanPlayersTurn model then
                        ( model
                        , dealCardToPlayer model
                        )

                    else
                        ( model, Cmd.none )

                DeclaringWildCardColor ->
                    ( model, Cmd.none )

                PlayerWonTheGame _ ->
                    ( model, Cmd.none )

        PlayerClickedCardInHand card ->
            case Pile.topCard model.pile of
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
                        ( model
                        , getBoundsOfCardAndPile
                            { card = card
                            , onComplete = AnimationHandToPileStep1
                            }
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

        AnimationDeckToHandStep1 { card, result } ->
            onAnimationStep1
                { model = model
                , result = result
                , toFloatingCard =
                    \{ cardElement, deckElement } ->
                        { card = card
                        , source = deckElement
                        , destination = cardElement
                        }
                , onFailureMsg = AnimationDeckToHandComplete
                }

        AnimationDeckToHandStep2 floatingCard ->
            onAnimationStep2
                { model = model
                , floatingCard = floatingCard
                , delayInMs = 500
                , onComplete = AnimationDeckToHandComplete
                }

        AnimationDeckToHandComplete ->
            ( drawAnotherCardIntoHand model
                |> checkIfDeckIsEmpty
                |> moveOnToNextPlayer Nothing
                |> removeFloatingCard
            , Cmd.none
            )

        AnimationHandToPileStep1 { card, result } ->
            onAnimationStep1
                { model = model
                , result = result
                , toFloatingCard =
                    \{ cardElement, pileElement } ->
                        { card = card
                        , source = cardElement
                        , destination = pileElement
                        }
                , onFailureMsg = AnimationHandToPileComplete card
                }

        AnimationHandToPileStep2 floatingCard ->
            onAnimationStep2
                { model = model
                , floatingCard = floatingCard
                , delayInMs = 800
                , onComplete = AnimationHandToPileComplete floatingCard.card
                }

        AnimationHandToPileComplete card ->
            ( playCardOntoPile card model
                |> checkIfGameOver
                |> checkIfPlayedWildCard card
                |> checkIfDeckIsEmpty
                |> checkIfDraw2OrDraw4 card
                |> moveOnToNextPlayer (Just card)
                |> removeFloatingCard
            , Cmd.none
            )


onAnimationStep1 :
    { result : Result Browser.Dom.Error value
    , model : Model
    , toFloatingCard : value -> FloatingCard
    , onFailureMsg : Msg
    }
    -> ( Model, Cmd Msg )
onAnimationStep1 ({ model } as options) =
    case options.result of
        Ok value ->
            let
                floatingCard =
                    options.toFloatingCard value
            in
            ( { model | floatingCardState = FloatingCardAtSource floatingCard }
            , sendMessageAfterDelay
                { delayInMs = 100
                , msg = AnimationDeckToHandStep2 floatingCard
                }
            )

        Err _ ->
            ( options.model
            , sendMessage options.onFailureMsg
            )


onAnimationStep2 :
    { floatingCard : FloatingCard
    , delayInMs : Int
    , onComplete : Msg
    , model : Model
    }
    -> ( Model, Cmd Msg )
onAnimationStep2 ({ model } as options) =
    ( { model | floatingCardState = FloatingCardAtDestination options.floatingCard }
    , sendMessageAfterDelay
        { delayInMs = options.delayInMs
        , msg = options.onComplete
        }
    )


dealCardToPlayer : Model -> Cmd Msg
dealCardToPlayer model =
    case ( Hand.lastCard model.playersHand, Deck.topCard model.deck ) of
        ( Just lastCardInPlayersHand, Just topCardInDeck ) ->
            getBoundsOfLastCardInPlayerHandAndDeck
                { lastCardInPlayersHand = lastCardInPlayersHand
                , topCardInDeck = topCardInDeck
                , onComplete = AnimationDeckToHandStep1
                }

        _ ->
            sendMessage AnimationDeckToHandComplete


removeFloatingCard : Model -> Model
removeFloatingCard model =
    { model | floatingCardState = NoFloatingCard }


sendMessage : msg -> Cmd msg
sendMessage msg =
    sendMessageAfterDelay
        { delayInMs = 0
        , msg = msg
        }


sendMessageAfterDelay : { delayInMs : Int, msg : msg } -> Cmd msg
sendMessageAfterDelay options =
    Process.sleep (toFloat options.delayInMs)
        |> Task.map (\_ -> options.msg)
        |> Task.perform (\msg -> msg)


getBoundsOfLastCardInPlayerHandAndDeck :
    { topCardInDeck : Card
    , lastCardInPlayersHand : Card
    , onComplete :
        { card : Card
        , result :
            Result
                Browser.Dom.Error
                { cardElement : BoundingRect
                , deckElement : BoundingRect
                }
        }
        -> msg
    }
    -> Cmd msg
getBoundsOfLastCardInPlayerHandAndDeck { topCardInDeck, lastCardInPlayersHand, onComplete } =
    let
        cardId : Card.Id
        cardId =
            Card.toUniqueId lastCardInPlayersHand

        task :
            Task
                Browser.Dom.Error
                { cardElement : BoundingRect
                , deckElement : BoundingRect
                }
        task =
            Task.map2
                (\card_ deck_ ->
                    { cardElement = card_.element
                    , deckElement = deck_.element
                    }
                )
                (Browser.Dom.getElement cardId)
                (Browser.Dom.getElement "deck")
    in
    task
        |> Task.attempt
            (\result ->
                onComplete
                    { card = topCardInDeck
                    , result = result
                    }
            )


getBoundsOfCardAndPile :
    { card : Card
    , onComplete :
        { card : Card
        , result :
            Result
                Browser.Dom.Error
                { cardElement : BoundingRect
                , pileElement : BoundingRect
                }
        }
        -> msg
    }
    -> Cmd msg
getBoundsOfCardAndPile { card, onComplete } =
    let
        cardId : Card.Id
        cardId =
            Card.toUniqueId card

        task :
            Task
                Browser.Dom.Error
                { cardElement : BoundingRect
                , pileElement : BoundingRect
                }
        task =
            Task.map2
                (\card_ pile_ ->
                    { cardElement = card_.element
                    , pileElement = pile_.element
                    }
                )
                (Browser.Dom.getElement cardId)
                (Browser.Dom.getElement "pile")
    in
    task
        |> Task.attempt
            (\result ->
                onComplete
                    { card = card
                    , result = result
                    }
            )


isHumanPlayersTurn : Model -> Bool
isHumanPlayersTurn model =
    0 == model.currentPlayerId


isComputersTurn : Model -> Bool
isComputersTurn model =
    model.currentPlayerId > 0


subscriptions : Model -> Sub Msg
subscriptions model =
    if isComputersTurn model then
        Time.every 1500 (\_ -> ComputerTakesTurn)

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

        afterDrawingToComputers :
            { deck : Deck
            , computerHands : Dict PlayerId Hand
            }
        afterDrawingToComputers =
            Dict.foldl loop
                { deck = afterDrawPlayerHand.deck
                , computerHands = model.computerHands
                }
                model.computerHands

        loop :
            PlayerId
            -> Hand
            ->
                { deck : Deck
                , computerHands : Dict PlayerId Hand
                }
            ->
                { deck : Deck
                , computerHands : Dict PlayerId Hand
                }
        loop playerId _ { deck, computerHands } =
            let
                afterDrawing =
                    Deck.draw 7 deck
            in
            { computerHands =
                Dict.insert playerId
                    (Hand.fromListOfCards afterDrawing.cards)
                    computerHands
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
                , playersHand = Hand.fromListOfCards afterDrawPlayerHand.cards
                , computerHands = afterDrawingToComputers.computerHands
                , pile =
                    case maybeCardToAddToPile of
                        Just card ->
                            Pile.empty
                                |> Pile.addCardToTop card

                        Nothing ->
                            Pile.empty
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
                            model.pile
                                |> Pile.addCardToTop card

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
                    model.playersHand
                        |> Hand.addCards [ card ]

                Nothing ->
                    model.playersHand
    }


playCardOntoPile : Card -> Model -> Model
playCardOntoPile card model =
    { model
        | playersHand = Hand.removeCard card model.playersHand
        , pile =
            model.pile
                |> Pile.addCardToTop card
    }


haveComputerTakeTurn : Model -> Model
haveComputerTakeTurn model =
    case ( Pile.topCard model.pile, Dict.get model.currentPlayerId model.computerHands ) of
        ( Just topCardOnPile, Just hand ) ->
            let
                playableCards : List Card
                playableCards =
                    Hand.findPlayableCards
                        model.declaredColor
                        topCardOnPile
                        hand

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

                computerDeclaredWild : Card -> List Card.Color -> Maybe Card.Color
                computerDeclaredWild card colorsOfRemainingCards =
                    if Card.isOneOfTheWildCards card then
                        Random.initialSeed model.seed
                            |> Random.step
                                (Random.List.choose
                                    (if List.isEmpty colorsOfRemainingCards then
                                        [ Card.Red, Card.Yellow, Card.Green, Card.Blue ]

                                     else
                                        colorsOfRemainingCards
                                    )
                                )
                            |> Tuple.first
                            |> Tuple.first

                    else
                        Nothing
            in
            case cardToPlay of
                Just card ->
                    let
                        computerHandsWithoutCard : Dict Int Hand
                        computerHandsWithoutCard =
                            removeCardFromComputersHand model.currentPlayerId
                                card
                                model.computerHands

                        colorsInRemainingHand : List Card.Color
                        colorsInRemainingHand =
                            computerHandsWithoutCard
                                |> Dict.get model.currentPlayerId
                                |> Maybe.map Hand.getAvailableColors
                                |> Maybe.withDefault []
                    in
                    { model
                        | seed = model.seed + 1
                        , computerHands = computerHandsWithoutCard
                        , pile =
                            model.pile
                                |> Pile.addCardToTop card
                        , declaredColor =
                            computerDeclaredWild card colorsInRemainingHand
                    }
                        |> checkIfDraw2OrDraw4 card
                        |> moveOnToNextPlayer (Just card)

                Nothing ->
                    let
                        afterComputerDraws =
                            Deck.draw 1 model.deck
                    in
                    { model
                        | seed = model.seed + 1
                        , computerHands =
                            addCardsToComputersHand model.currentPlayerId
                                afterComputerDraws.cards
                                model.computerHands
                        , deck = afterComputerDraws.deck
                    }
                        |> moveOnToNextPlayer Nothing

        _ ->
            model


removeCardFromComputersHand : PlayerId -> Card -> Dict PlayerId Hand -> Dict PlayerId Hand
removeCardFromComputersHand id card computerHands =
    Dict.update id
        (\maybeComputersHand ->
            case maybeComputersHand of
                Just computersHand ->
                    Just (Hand.removeCard card computersHand)

                Nothing ->
                    Nothing
        )
        computerHands


addCardsToComputersHand : PlayerId -> List Card -> Dict PlayerId Hand -> Dict PlayerId Hand
addCardsToComputersHand id newCards computerHands =
    Dict.update id
        (\maybeComputersHand ->
            case maybeComputersHand of
                Just computersHand ->
                    Just (computersHand |> Hand.addCards newCards)

                Nothing ->
                    Nothing
        )
        computerHands


checkIfDraw2OrDraw4 : Card -> Model -> Model
checkIfDraw2OrDraw4 card model =
    let
        nextPlayerId : PlayerId
        nextPlayerId =
            getNextPlayerId
                { total = getTotalPlayers model
                , distance = 1
                , direction = model.direction
                , currentPlayerId = model.currentPlayerId
                }

        giveCards : Int -> Model
        giveCards amount =
            let
                afterDrawing : { cards : List Card, deck : Deck }
                afterDrawing =
                    Deck.draw amount model.deck
            in
            case nextPlayerId of
                0 ->
                    { model
                        | playersHand =
                            model.playersHand
                                |> Hand.addCards afterDrawing.cards
                        , deck = afterDrawing.deck
                    }

                computerId ->
                    { model
                        | computerHands =
                            addCardsToComputersHand computerId
                                afterDrawing.cards
                                model.computerHands
                        , deck = afterDrawing.deck
                    }
    in
    if Card.isDrawTwo card then
        giveCards 2

    else if Card.isWildDraw4 card then
        giveCards 4

    else
        model


getTotalPlayers : Model -> Int
getTotalPlayers model =
    1 + Dict.size model.computerHands


checkIfGameOver : Model -> Model
checkIfGameOver model =
    if Hand.isEmpty model.playersHand then
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
        (\_ hand -> Hand.isEmpty hand)
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
            ( bottomCardsOfPile, newPile ) =
                Pile.removeBottomCards model.pile

            newDeck : Deck
            newDeck =
                Deck.reshuffle
                    (Random.initialSeed model.seed)
                    bottomCardsOfPile
        in
        { model
            | deck = newDeck
            , seed = model.seed + 1
            , pile = newPile
        }

    else
        model


moveOnToNextPlayer : Maybe Card -> Model -> Model
moveOnToNextPlayer maybeCardPlayed model =
    let
        totalPlayerCount : Int
        totalPlayerCount =
            getTotalPlayers model
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


getNextPlayerId :
    { distance : Int
    , direction : Direction
    , total : Int
    , currentPlayerId : PlayerId
    }
    -> PlayerId
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
        , viewFloatingCard model
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
        [ Pile.view model.pile
        , Deck.view
            { onClick = PlayerClickedDeck
            , deck = model.deck
            }
        , viewDeclaredColor model
        ]


viewDeclaredColor : Model -> Html Msg
viewDeclaredColor model =
    case model.declaredColor of
        Just color ->
            Html.div [ Html.Attributes.class "declared-color" ] [ Html.text (Card.colorToName color) ]

        Nothing ->
            Html.text ""


viewComputerHands : Model -> Html Msg
viewComputerHands model =
    let
        viewComputerHand : Int -> Hand.Side -> Html Msg
        viewComputerHand id side =
            case Dict.get id model.computerHands of
                Just hand ->
                    Hand.view
                        { cardToHide = toMaybeFloatingCard model.floatingCardState
                        , side = side
                        , hand = hand
                        , isCurrentlyPlaying = id == model.currentPlayerId
                        , shouldHideCards = True
                        , onClick = PlayerClickedCardInHand
                        }

                Nothing ->
                    Html.text ""
    in
    Html.div [ Html.Attributes.class "computer-hands" ]
        [ viewComputerHand 1 Hand.Left
        , viewComputerHand 2 Hand.Top
        , viewComputerHand 3 Hand.Right
        ]


toMaybeFloatingCard : FloatingCardState -> Maybe Card
toMaybeFloatingCard floatingCardState =
    case floatingCardState of
        NoFloatingCard ->
            Nothing

        FloatingCardAtSource { card } ->
            Just card

        FloatingCardAtDestination { card } ->
            Just card


viewPlayerHand : Model -> Html Msg
viewPlayerHand model =
    Hand.view
        { cardToHide = toMaybeFloatingCard model.floatingCardState
        , side = Hand.Bottom
        , hand = model.playersHand
        , isCurrentlyPlaying = 0 == model.currentPlayerId
        , shouldHideCards = False
        , onClick = PlayerClickedCardInHand
        }


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
viewDeclareColorDialog _ =
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
viewYouWonDialog _ =
    viewDialogWithContent
        [ Html.p [ Html.Attributes.class "dialog__message" ] [ Html.text "You won!" ]
        , Html.button
            [ Html.Events.onClick PlayerClickedPlayAgain
            , Html.Attributes.class "button"
            ]
            [ Html.text "Play again!" ]
        ]


viewYouLostDialog : PlayerId -> Model -> Html Msg
viewYouLostDialog idOfPlayerWhoWon _ =
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


viewFloatingCard : Model -> Html msg
viewFloatingCard model =
    let
        viewCardAtRectangle : Card -> BoundingRect -> Html msg
        viewCardAtRectangle card rect =
            Html.div
                [ Html.Attributes.class "floating-card"
                , Html.Attributes.style "top" (px rect.y)
                , Html.Attributes.style "left" (px rect.x)
                , Html.Attributes.style "width" (px rect.width)
                , Html.Attributes.style "height" (px rect.height)
                ]
                [ Card.view card
                ]
    in
    case model.floatingCardState of
        NoFloatingCard ->
            Html.text ""

        FloatingCardAtSource { card, source } ->
            viewCardAtRectangle card source

        FloatingCardAtDestination { card, destination } ->
            viewCardAtRectangle card destination


px : Float -> String
px float =
    String.fromFloat float ++ "px"
