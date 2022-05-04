module Main exposing (Model, Msg, init, initWithSettings, main, subscriptions, update, view)

import Browser
import Browser.Dom
import Card exposing (Card)
import Deck exposing (Deck)
import Dict exposing (Dict)
import Hand exposing (Hand)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import NonEmptyList exposing (NonEmptyList)
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
    { cardsToDraw : Int
    , phase : Phase
    , deck : Deck
    , pile : Pile
    , direction : Direction
    , currentPlayerId : Int
    , playersHand : Hand
    , computerHands : Dict PlayerId Hand
    , declaredColor : Maybe Card.Color
    , floatingCardState : FloatingCardState
    , seed : Int
    , alert : Maybe Alert
    , timeElapsedSinceLastTurn :
        { playerId : PlayerId
        , time : Float
        }
    }


type Alert
    = SkipAlert
    | ReverseAlert


type FloatingCardState
    = NoFloatingCard
    | FloatingCardAtSource FloatingCard
    | FloatingCardAtDestination FloatingCard


type alias FloatingCard =
    { cards : NonEmptyList Card
    , source : BoundingRect
    , destination : BoundingRect
    , shouldHide : Bool
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
    | DeclaringWildCardColor Card
    | PlayerWonTheGame Int


initWithSettings : { deck : Deck, cardsToDraw : Int } -> Flags -> ( Model, Cmd Msg )
initWithSettings options flags =
    ( { cardsToDraw = options.cardsToDraw
      , phase = ReadyToPlay
      , deck = options.deck
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
      , alert = Nothing
      , timeElapsedSinceLastTurn =
            { playerId = 0
            , time = 0
            }
      }
    , Cmd.none
    )


init : Flags -> ( Model, Cmd Msg )
init flags =
    initWithSettings
        { cardsToDraw = 7
        , deck = Deck.new (Random.initialSeed 0)
        }
        flags



-- UPDATE


type Msg
    = PlayerClickedDeck
    | PlayerClickedCardInHand Card
    | PlayerClickedPlayAgain
    | PlayerDeclaredColor Card Card.Color
      -- Animations
    | DetectedChangeInTurn
    | ComputerTakesTurn
      -- Player hand to pile animation
    | AnimationPlayerHandToPileStep1 CardAndPilePayload
    | AnimationPlayerHandToPileStep2 FloatingCard
    | AnimationPlayerHandToPileComplete Card
      -- Deck to player hand animation
    | AnimationPlayerDeckToHandStep1 CardAndDeckPayload
    | AnimationPlayerDeckToHandStep2 FloatingCard
    | AnimationPlayerDeckToHandComplete
      -- Computer hand to pile animation
    | AnimationComputerHandToPileStep1 ComputerAction CardAndPilePayload
    | AnimationComputerHandToPileStep2 ComputerAction FloatingCard
      -- Deck to computer hand animation
    | AnimationComputerDeckToHandStep1 ComputerAction CardAndDeckPayload
    | AnimationComputerDeckToHandStep2 ComputerAction FloatingCard
    | AnimationComputerComplete ComputerAction
      -- Draw 2 / Draw 4 animation
    | DrawAnimationStart DrawAnimationPayload
    | DrawAnimationStep1 DrawAnimationPayload CardAndDeckPayload
    | DrawAnimationStep2 DrawAnimationPayload FloatingCard
    | DrawAnimationComplete DrawAnimationPayload
      -- Sick alerts
    | SubscriptionDismissedAlert


type alias DrawAnimationPayload =
    { model : Model
    , drawAction : DrawCardAction
    }


type alias CardAndPilePayload =
    { card : Card
    , result :
        Result
            Browser.Dom.Error
            { cardElement : BoundingRect
            , pileElement : BoundingRect
            }
    }


type alias CardAndDeckPayload =
    { cardsFromDeck : NonEmptyList Card
    , result :
        Result
            Browser.Dom.Error
            { cardElement : BoundingRect
            , deckElement : BoundingRect
            }
    }


type alias BoundingRect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update =
    detectTurnOrderChange update_


update_ : Msg -> Model -> ( Model, Cmd Msg )
update_ msg model =
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

                DeclaringWildCardColor _ ->
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
                            , onComplete = AnimationPlayerHandToPileStep1
                            }
                        )

                    else
                        ( model, Cmd.none )

        PlayerDeclaredColor playedWildCard color ->
            ( { model
                | declaredColor = Just color
                , phase = PlayingGame
              }
                |> moveOnToNextPlayer (Just playedWildCard)
            , Cmd.none
            )

        PlayerClickedPlayAgain ->
            ( startNewGame model, Cmd.none )

        ComputerTakesTurn ->
            case model.phase of
                ReadyToPlay ->
                    ( model, Cmd.none )

                PlayingGame ->
                    let
                        action : ComputerAction
                        action =
                            haveComputerTakeTurn model
                    in
                    case action of
                        ComputerCannotTakeAction ->
                            performComputerAction action model

                        ComputerPlaysCard card ->
                            ( model
                            , getBoundsOfCardAndPile
                                { card = card
                                , onComplete = AnimationComputerHandToPileStep1 action
                                }
                            )

                        ComputerDrawsFromDeck computerHand ->
                            case ( Hand.lastCard computerHand, Deck.topCard model.deck ) of
                                ( Just lastCardInHand, Just topCardInDeck ) ->
                                    ( model
                                    , getBoundsOfLastCardInHandAndDeck
                                        { lastCardInHand = lastCardInHand
                                        , cardsFromDeck = NonEmptyList.new topCardInDeck []
                                        , onComplete = AnimationComputerDeckToHandStep1 action
                                        }
                                    )

                                _ ->
                                    ( model, Cmd.none )

                DeclaringWildCardColor _ ->
                    ( model, Cmd.none )

                PlayerWonTheGame _ ->
                    ( model, Cmd.none )

        AnimationComputerHandToPileStep1 action { card, result } ->
            onAnimationStep1
                { model = model
                , result = result
                , toFloatingCard =
                    \{ cardElement, pileElement } ->
                        { cards = NonEmptyList.new card []
                        , shouldHide = False
                        , source = cardElement
                        , destination = pileElement
                        }
                , onSuccessMsg = AnimationComputerHandToPileStep2 action
                , onFailureMsg = AnimationComputerComplete action
                }

        AnimationComputerHandToPileStep2 action floatingCard ->
            onAnimationStep2
                { model = model
                , floatingCard = floatingCard
                , delayInMs = 800
                , onComplete = AnimationComputerComplete action
                }

        AnimationComputerDeckToHandStep1 action { cardsFromDeck, result } ->
            onAnimationStep1
                { model = model
                , result = result
                , toFloatingCard =
                    \{ cardElement, deckElement } ->
                        { cards = cardsFromDeck
                        , shouldHide = True
                        , source = deckElement
                        , destination = cardElement
                        }
                , onSuccessMsg = AnimationComputerDeckToHandStep2 action
                , onFailureMsg = AnimationComputerComplete action
                }

        AnimationComputerDeckToHandStep2 action floatingCard ->
            onAnimationStep2
                { model = model
                , floatingCard = floatingCard
                , delayInMs = 500
                , onComplete = AnimationComputerComplete action
                }

        AnimationComputerComplete action ->
            performComputerAction action model

        AnimationPlayerDeckToHandStep1 { cardsFromDeck, result } ->
            onAnimationStep1
                { model = model
                , result = result
                , toFloatingCard =
                    \{ cardElement, deckElement } ->
                        { cards = cardsFromDeck
                        , shouldHide = False
                        , source = deckElement
                        , destination = cardElement
                        }
                , onSuccessMsg = AnimationPlayerDeckToHandStep2
                , onFailureMsg = AnimationPlayerDeckToHandComplete
                }

        AnimationPlayerDeckToHandStep2 floatingCard ->
            onAnimationStep2
                { model = model
                , floatingCard = floatingCard
                , delayInMs = 500
                , onComplete = AnimationPlayerDeckToHandComplete
                }

        AnimationPlayerDeckToHandComplete ->
            ( drawAnotherCardIntoHand model
                |> checkIfDeckIsEmpty
                |> moveOnToNextPlayer Nothing
                |> removeFloatingCard
            , Cmd.none
            )

        AnimationPlayerHandToPileStep1 { card, result } ->
            onAnimationStep1
                { model = model
                , result = result
                , toFloatingCard =
                    \{ cardElement, pileElement } ->
                        { cards = NonEmptyList.new card []
                        , shouldHide = False
                        , source = cardElement
                        , destination = pileElement
                        }
                , onSuccessMsg = AnimationPlayerHandToPileStep2
                , onFailureMsg = AnimationPlayerHandToPileComplete card
                }

        AnimationPlayerHandToPileStep2 floatingCard ->
            onAnimationStep2
                { model = model
                , floatingCard = floatingCard
                , delayInMs = 800
                , onComplete = AnimationPlayerHandToPileComplete (NonEmptyList.head floatingCard.cards)
                }

        AnimationPlayerHandToPileComplete card ->
            let
                updatedModel : Model
                updatedModel =
                    playCardOntoPile card model
                        |> checkIfGameOver
                        |> checkIfPlayedWildCard card
                        |> checkIfDeckIsEmpty
                        |> removeFloatingCard
                        |> checkForAlert card
            in
            case toMaybeDrawAction card of
                Just drawAction ->
                    ( model
                    , startDrawAnimation
                        { model = updatedModel
                        , drawAction = drawAction
                        }
                    )

                Nothing ->
                    ( updatedModel, Cmd.none )

        DrawAnimationStart payload ->
            let
                nextPlayerId : PlayerId
                nextPlayerId =
                    getNextPlayerId
                        { distance = 1
                        , direction = model.direction
                        , total = getTotalPlayers model
                        , currentPlayerId = model.currentPlayerId
                        }

                nextPlayersHand : Hand
                nextPlayersHand =
                    case nextPlayerId of
                        0 ->
                            model.playersHand

                        computerId ->
                            Dict.get computerId model.computerHands
                                |> Maybe.withDefault Hand.empty

                topCardsToDraw : List Card
                topCardsToDraw =
                    case payload.drawAction of
                        DrawTwoPlayed ->
                            Deck.topCards 2 model.deck

                        DrawFourPlayed ->
                            Deck.topCards 4 model.deck
            in
            case ( Hand.lastCard nextPlayersHand, NonEmptyList.fromList topCardsToDraw ) of
                ( Just lastCardInHand, Just cardsFromDeck ) ->
                    ( model
                    , getBoundsOfLastCardInHandAndDeck
                        { lastCardInHand = lastCardInHand
                        , cardsFromDeck = cardsFromDeck
                        , onComplete = DrawAnimationStep1 payload
                        }
                    )

                _ ->
                    ( model
                    , sendMessage (DrawAnimationComplete payload)
                    )

        DrawAnimationStep1 payload { cardsFromDeck, result } ->
            onAnimationStep1
                { model = model
                , result = result
                , toFloatingCard =
                    \{ cardElement, deckElement } ->
                        { cards = cardsFromDeck
                        , shouldHide = True
                        , source = deckElement
                        , destination = cardElement
                        }
                , onSuccessMsg = DrawAnimationStep2 payload
                , onFailureMsg = DrawAnimationComplete payload
                }

        DrawAnimationStep2 payload floatingCard ->
            onAnimationStep2
                { model = model
                , floatingCard = floatingCard
                , delayInMs = 600
                , onComplete = DrawAnimationComplete payload
                }

        DrawAnimationComplete payload ->
            ( payload.model
                |> performDrawCardAction payload.drawAction
                |> removeFloatingCard
            , Cmd.none
            )

        SubscriptionDismissedAlert ->
            ( { model | alert = Nothing }
            , Cmd.none
            )

        DetectedChangeInTurn ->
            ( model
            , Cmd.none
            )


startDrawAnimation : DrawAnimationPayload -> Cmd Msg
startDrawAnimation payload =
    sendMessage (DrawAnimationStart payload)


onAnimationStep1 :
    { result : Result Browser.Dom.Error value
    , model : Model
    , toFloatingCard : value -> FloatingCard
    , onSuccessMsg : FloatingCard -> Msg
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
                , msg = options.onSuccessMsg floatingCard
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
        ( Just lastCardInHand, Just topCardInDeck ) ->
            getBoundsOfLastCardInHandAndDeck
                { lastCardInHand = lastCardInHand
                , cardsFromDeck = NonEmptyList.new topCardInDeck []
                , onComplete = AnimationPlayerDeckToHandStep1
                }

        _ ->
            sendMessage AnimationPlayerDeckToHandComplete


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


getBoundsOfLastCardInHandAndDeck :
    { cardsFromDeck : NonEmptyList Card
    , lastCardInHand : Card
    , onComplete :
        { cardsFromDeck : NonEmptyList Card
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
getBoundsOfLastCardInHandAndDeck { cardsFromDeck, lastCardInHand, onComplete } =
    let
        cardId : Card.Id
        cardId =
            Card.toUniqueId lastCardInHand

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
                    { cardsFromDeck = cardsFromDeck
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
    Sub.batch
        [ if model.alert /= Nothing then
            Time.every 800 (\_ -> SubscriptionDismissedAlert)

          else
            Sub.none
        ]


startNewGame : Model -> Model
startNewGame model =
    let
        { cardsToDraw } =
            model

        newDeck : Deck
        newDeck =
            Deck.new (Random.initialSeed model.seed)

        afterDrawPlayerHand : { cards : List Card, deck : Deck }
        afterDrawPlayerHand =
            Deck.draw cardsToDraw newDeck

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
                    Deck.draw cardsToDraw deck
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


haveComputerTakeTurn : Model -> ComputerAction
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
            in
            case cardToPlay of
                Just card ->
                    ComputerPlaysCard card

                Nothing ->
                    case Dict.get model.currentPlayerId model.computerHands of
                        Just computerHand ->
                            ComputerDrawsFromDeck computerHand

                        Nothing ->
                            ComputerCannotTakeAction

        _ ->
            ComputerCannotTakeAction


type ComputerAction
    = ComputerPlaysCard Card
    | ComputerDrawsFromDeck Hand
    | ComputerCannotTakeAction


performComputerAction : ComputerAction -> Model -> ( Model, Cmd Msg )
performComputerAction computerAction model =
    case computerAction of
        ComputerCannotTakeAction ->
            ( model, Cmd.none )

        ComputerPlaysCard card ->
            let
                computerHandsWithoutCard : Dict Int Hand
                computerHandsWithoutCard =
                    removeCardFromComputersHand model.currentPlayerId
                        card
                        model.computerHands

                remainingColorsInHand : List Card.Color
                remainingColorsInHand =
                    computerHandsWithoutCard
                        |> Dict.get model.currentPlayerId
                        |> Maybe.map Hand.getAvailableColors
                        |> Maybe.withDefault []

                computerDeclaredWild : List Card.Color -> Maybe Card.Color
                computerDeclaredWild colorsOfRemainingCards =
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

                updatedModel : Model
                updatedModel =
                    { model
                        | seed = model.seed + 1
                        , computerHands = computerHandsWithoutCard
                        , pile =
                            model.pile
                                |> Pile.addCardToTop card
                        , declaredColor =
                            computerDeclaredWild remainingColorsInHand
                    }
                        |> moveOnToNextPlayer (Just card)
                        |> checkIfGameOver
                        |> checkIfDeckIsEmpty
                        |> removeFloatingCard
                        |> checkForAlert card
            in
            case toMaybeDrawAction card of
                Just drawAction ->
                    ( model
                    , startDrawAnimation
                        { model = updatedModel
                        , drawAction = drawAction
                        }
                    )

                Nothing ->
                    ( updatedModel
                    , Cmd.none
                    )

        ComputerDrawsFromDeck _ ->
            let
                afterComputerDraws : { cards : List Card, deck : Deck }
                afterComputerDraws =
                    Deck.draw 1 model.deck

                updatedModel : Model
                updatedModel =
                    { model
                        | seed = model.seed + 1
                        , computerHands =
                            addCardsToComputersHand model.currentPlayerId
                                afterComputerDraws.cards
                                model.computerHands
                        , deck = afterComputerDraws.deck
                    }
                        |> moveOnToNextPlayer Nothing
                        |> checkIfGameOver
                        |> checkIfDeckIsEmpty
                        |> removeFloatingCard
            in
            ( updatedModel
            , Cmd.none
            )


checkForAlert : Card -> Model -> Model
checkForAlert card model =
    if Card.isSkip card then
        { model | alert = Just SkipAlert }

    else if Card.isReverse card then
        { model | alert = Just ReverseAlert }

    else
        { model | alert = Nothing }


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


toMaybeDrawAction : Card -> Maybe DrawCardAction
toMaybeDrawAction card =
    if Card.isDrawTwo card then
        Just DrawTwoPlayed

    else if Card.isWildDraw4 card then
        Just DrawFourPlayed

    else
        Nothing


type DrawCardAction
    = DrawTwoPlayed
    | DrawFourPlayed


performDrawCardAction : DrawCardAction -> Model -> Model
performDrawCardAction action model =
    let
        skippedPlayerId : PlayerId
        skippedPlayerId =
            getNextPlayerId
                { total = getTotalPlayers model
                , distance = 1
                , direction = reverseDirection model.direction
                , currentPlayerId = model.currentPlayerId
                }

        giveCards : Int -> Model
        giveCards amount =
            let
                afterDrawing : { cards : List Card, deck : Deck }
                afterDrawing =
                    Deck.draw amount model.deck
            in
            case skippedPlayerId of
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
    case action of
        DrawTwoPlayed ->
            giveCards 2

        DrawFourPlayed ->
            giveCards 4


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
                { model | phase = DeclaringWildCardColor card }

            else
                { model | declaredColor = Nothing }
                    |> moveOnToNextPlayer (Just card)

        _ ->
            { model | declaredColor = Nothing }
                |> moveOnToNextPlayer (Just card)


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
        , viewSickAlert model
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

        FloatingCardAtSource { cards } ->
            Just (NonEmptyList.head cards)

        FloatingCardAtDestination { cards } ->
            Just (NonEmptyList.head cards)


viewPlayerHand : Model -> Html Msg
viewPlayerHand model =
    Html.div [ Html.Attributes.class "player-hand" ]
        [ Hand.view
            { cardToHide = toMaybeFloatingCard model.floatingCardState
            , side = Hand.Bottom
            , hand = model.playersHand
            , isCurrentlyPlaying = 0 == model.currentPlayerId
            , shouldHideCards = False
            , onClick = PlayerClickedCardInHand
            }
        ]


viewDialog : Model -> Html Msg
viewDialog model =
    case model.phase of
        DeclaringWildCardColor card ->
            viewDeclareColorDialog card

        PlayerWonTheGame 0 ->
            viewYouWonDialog model

        PlayerWonTheGame id ->
            viewYouLostDialog id model

        _ ->
            Html.text ""


viewDeclareColorDialog : Card -> Html Msg
viewDeclareColorDialog card =
    let
        viewChooseColorButton color =
            Html.button
                [ Html.Events.onClick (PlayerDeclaredColor card color)
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
        viewCardAtRectangle : ( Card, List Card ) -> Bool -> BoundingRect -> Html msg
        viewCardAtRectangle ( first, otherCards ) shouldHide rect =
            let
                viewSingleCard : Card -> Html msg
                viewSingleCard card =
                    if shouldHide then
                        Card.viewBackOfCard card

                    else
                        Card.view card

                offset : Int
                offset =
                    10

                viewOffsetCards : List (Html msg)
                viewOffsetCards =
                    [ viewSingleCard first
                    , List.indexedMap viewOffsetCard otherCards
                        |> Html.div []
                    ]

                viewOffsetCard : Int -> Card -> Html msg
                viewOffsetCard i card =
                    Html.div
                        [ Html.Attributes.class "floating-card__copy"
                        , Html.Attributes.style "top" (pct (offset * (1 + i)))
                        , Html.Attributes.style "left" (pct (offset * (1 + i)))
                        ]
                        [ viewSingleCard card ]
            in
            Html.div
                [ Html.Attributes.class "floating-card"
                , Html.Attributes.style "top" (px rect.y)
                , Html.Attributes.style "left" (px rect.x)
                , Html.Attributes.style "width" (px rect.width)
                , Html.Attributes.style "height" (px rect.height)
                ]
                viewOffsetCards
    in
    case model.floatingCardState of
        NoFloatingCard ->
            Html.text ""

        FloatingCardAtSource { cards, shouldHide, source } ->
            viewCardAtRectangle cards shouldHide source

        FloatingCardAtDestination { cards, shouldHide, destination } ->
            viewCardAtRectangle cards shouldHide destination


px : Float -> String
px float =
    String.fromFloat float ++ "px"


pct : Int -> String
pct int =
    String.fromInt int ++ "%"


viewSickAlert : Model -> Html Msg
viewSickAlert model =
    Html.div [ Html.Attributes.style "pointer-events" "none" ]
        [ viewAlert
            { isVisible = model.alert == Just SkipAlert
            , message = "Skip!"
            }
        , viewAlert
            { isVisible = model.alert == Just ReverseAlert
            , message = "Reverse!"
            }
        ]


viewAlert : { isVisible : Bool, message : String } -> Html Msg
viewAlert { isVisible, message } =
    Html.div
        [ Html.Attributes.class "sick-alert"
        , Html.Attributes.classList
            [ ( "sick-alert--visible"
              , isVisible
              )
            ]
        ]
        [ Html.div
            [ Html.Attributes.class "sick-alert__message"
            ]
            [ Html.text message ]
        ]



-- Time.every workaround


{-| Was previously using `Time.every` to make the computers move every 1.5 seconds.
This meant they were unaware if an animation was in progress, so sometimes it felt
like the AI was moving quickly (after an animation) and other times it felt like
they were moving slowly (when no animation occurred)

The normal solution to this problem would be using something like
`Browser.Events.onAnimationFrame` to have smoother animations, but I wanted to
preserve your ability to use the Elm debugger, without logging a message on
every tick.

This meant detecting when any update changed the turn order to a computers turn,
which only happens at the end of an animation.

-}
detectTurnOrderChange :
    (Msg -> Model -> ( Model, Cmd Msg ))
    -> Msg
    -> Model
    -> ( Model, Cmd Msg )
detectTurnOrderChange originalUpdateFn msg model =
    let
        ( updatedModel, cmd ) =
            originalUpdateFn msg model

        turnOrderChanged : Bool
        turnOrderChanged =
            model.currentPlayerId /= updatedModel.currentPlayerId
    in
    if turnOrderChanged && isComputersTurn updatedModel then
        ( updatedModel
        , Cmd.batch
            [ sendMessageAfterDelay
                { delayInMs = 1000
                , msg = ComputerTakesTurn
                }
            , cmd
            ]
        )

    else
        ( updatedModel, cmd )
