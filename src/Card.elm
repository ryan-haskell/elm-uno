module Card exposing
    ( Card, unshuffledDeck
    , shuffle
    , isOkayToPlay
    , isReverse, isSkip, isDrawTwo, isWildDraw4
    , isOneOfTheWildCards
    , view, viewBackOfCard, viewEmptyDeck, viewEmptyPile
    , Id, toUniqueId
    , Color(..), colorToName, getColorsForCards
    , blueSkipCard, redDrawTwoCard
    )

{-|

@docs Card, unshuffledDeck
@docs shuffle

@docs isOkayToPlay
@docs isReverse, isSkip, isDrawTwo, isWildDraw4
@docs isOneOfTheWildCards

@docs view, viewBackOfCard, viewEmptyDeck, viewEmptyPile

@docs Id, toUniqueId

@docs Color, colorToName, getColorsForCards


### testing

@docs blueSkipCard, redDrawTwoCard

-}

import Html exposing (Html)
import Random
import Random.List
import Svg exposing (Svg)
import Svg.Attributes


type Card
    = Card Internals


type alias Id =
    String


type alias Internals =
    { id : Id
    , kind : CardKind
    }


toUniqueId : Card -> String
toUniqueId (Card card) =
    card.id


type CardKind
    = NumberCard { value : Int, color : Color }
    | ReverseCard { color : Color }
    | SkipCard { color : Color }
    | DrawTwoCard { color : Color }
    | WildCard
    | WildDraw4Card


isOkayToPlay : { declaredColor : Maybe Color, topCardOnPile : Card, cardFromHand : Card } -> Bool
isOkayToPlay options =
    let
        { topCardOnPile, cardFromHand } =
            { topCardOnPile = toBasicInformation options.topCardOnPile
            , cardFromHand = toBasicInformation options.cardFromHand
            }

        haveSameValue : Bool
        haveSameValue =
            case ( topCardOnPile.value, cardFromHand.value ) of
                ( Just value1, Just value2 ) ->
                    value1 == value2

                _ ->
                    False

        haveSameColor : Bool
        haveSameColor =
            case ( topCardOnPile.color, cardFromHand.color ) of
                ( Just color1, Just color2 ) ->
                    color1 == color2

                _ ->
                    False

        cardFromHandIsWild : Bool
        cardFromHandIsWild =
            cardFromHand.isWild

        cardFromHandMatchesDeclaredColor : Bool
        cardFromHandMatchesDeclaredColor =
            case ( options.declaredColor, cardFromHand.color ) of
                ( Just color1, Just color2 ) ->
                    color1 == color2

                _ ->
                    False
    in
    List.any (\condition -> condition == True)
        [ haveSameValue
        , haveSameColor
        , cardFromHandIsWild
        , cardFromHandMatchesDeclaredColor
        ]


isOneOfTheWildCards : Card -> Bool
isOneOfTheWildCards card =
    toBasicInformation card
        |> .isWild


isReverse : Card -> Bool
isReverse (Card card) =
    case card.kind of
        ReverseCard _ ->
            True

        _ ->
            False


isSkip : Card -> Bool
isSkip (Card card) =
    case card.kind of
        SkipCard _ ->
            True

        _ ->
            False


isDrawTwo : Card -> Bool
isDrawTwo (Card card) =
    case card.kind of
        DrawTwoCard _ ->
            True

        _ ->
            False


isWildDraw4 : Card -> Bool
isWildDraw4 (Card card) =
    case card.kind of
        WildDraw4Card ->
            True

        _ ->
            False


type Value
    = NumberValue Int
    | SkipValue
    | ReverseValue
    | DrawTwoValue


toBasicInformation :
    Card
    ->
        { value : Maybe Value
        , color : Maybe Color
        , isWild : Bool
        }
toBasicInformation (Card card) =
    case card.kind of
        NumberCard { value, color } ->
            { value = Just (NumberValue value)
            , color = Just color
            , isWild = False
            }

        SkipCard { color } ->
            { value = Just SkipValue
            , color = Just color
            , isWild = False
            }

        ReverseCard { color } ->
            { value = Just ReverseValue
            , color = Just color
            , isWild = False
            }

        DrawTwoCard { color } ->
            { value = Just DrawTwoValue
            , color = Just color
            , isWild = False
            }

        WildCard ->
            { value = Nothing
            , color = Nothing
            , isWild = True
            }

        WildDraw4Card ->
            { value = Nothing
            , color = Nothing
            , isWild = True
            }


view : Card -> Html msg
view (Card card) =
    case card.kind of
        NumberCard { value, color } ->
            viewNumberCard card.id
                { value = value
                , color = toHexColor color
                }

        ReverseCard { color } ->
            viewReverseCard card.id
                { color = toHexColor color
                }

        SkipCard { color } ->
            viewSkipCard card.id
                { color = toHexColor color
                }

        DrawTwoCard { color } ->
            viewDrawTwoCard card.id
                { color = toHexColor color
                }

        WildCard ->
            viewWildCard card.id

        WildDraw4Card ->
            viewWildDraw4Card card.id


viewBackOfCard : Card -> Html msg
viewBackOfCard card =
    viewCardWithIcon
        { id = toUniqueId card
        , color = black
        , viewCenterIcon = viewUnoCardBack
        , viewCornerIcon = Svg.g [] []
        }


viewEmptyPile : Html msg
viewEmptyPile =
    viewEmptyStackWithLabel "Pile"


viewEmptyDeck : Html msg
viewEmptyDeck =
    viewEmptyStackWithLabel "Deck"


viewEmptyStackWithLabel : String -> Html msg
viewEmptyStackWithLabel label =
    Svg.svg
        [ Svg.Attributes.class "card"
        , Svg.Attributes.viewBox "0 0 5.5 9"
        , Svg.Attributes.width "200px"
        ]
        [ Svg.rect
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.x "0.25"
            , Svg.Attributes.y "0.25"
            , Svg.Attributes.width "5"
            , Svg.Attributes.height "8.5"
            , Svg.Attributes.rx "0.25"
            , Svg.Attributes.stroke white
            , Svg.Attributes.strokeDasharray "0.5"
            , Svg.Attributes.strokeWidth "0.25"
            ]
            []
        , Svg.text_
            [ Svg.Attributes.class "card__empty-label"
            , Svg.Attributes.x "50%"
            , Svg.Attributes.y "50%"
            , Svg.Attributes.fill white
            , Svg.Attributes.dominantBaseline "middle"
            , Svg.Attributes.textAnchor "middle"
            ]
            [ Svg.text label
            ]
        ]


nameOfTheGame : String
nameOfTheGame =
    "NO U!"


viewUnoCardBack : Svg msg
viewUnoCardBack =
    Svg.g []
        [ Svg.ellipse
            [ Svg.Attributes.fill red
            , Svg.Attributes.cx "50%"
            , Svg.Attributes.cy "50%"
            , Svg.Attributes.width "5"
            , Svg.Attributes.height "8.5"
            , Svg.Attributes.rx "30%"
            , Svg.Attributes.ry "40%"
            , Svg.Attributes.stroke red
            , Svg.Attributes.strokeWidth "0.35"
            , Svg.Attributes.style "transform: skewX(-23deg) translateX(35%); "
            ]
            []
        , Svg.g [ Svg.Attributes.style "transform-origin: center; transform: scale(0.37) rotate(-10deg); " ]
            [ Svg.text_
                [ Svg.Attributes.class "card__big-number"
                , Svg.Attributes.x "50%"
                , Svg.Attributes.y "55%"
                , Svg.Attributes.fill black
                , Svg.Attributes.dominantBaseline "middle"
                , Svg.Attributes.textAnchor "middle"
                ]
                [ Svg.text nameOfTheGame
                ]

            -- big center number
            , Svg.text_
                [ Svg.Attributes.class "card__big-number"
                , Svg.Attributes.x "53%"
                , Svg.Attributes.y "53%"
                , Svg.Attributes.fill yellow
                , Svg.Attributes.dominantBaseline "middle"
                , Svg.Attributes.textAnchor "middle"
                , Svg.Attributes.stroke black
                , Svg.Attributes.strokeWidth "0.1"
                ]
                [ Svg.text nameOfTheGame
                ]
            ]
        ]


type Color
    = Red
    | Green
    | Yellow
    | Blue


colorToName : Color -> String
colorToName color =
    case color of
        Red ->
            "Red"

        Yellow ->
            "Yellow"

        Green ->
            "Green"

        Blue ->
            "Blue"


getColorsForCards : List Card -> List Color
getColorsForCards cards =
    cards
        |> List.map toBasicInformation
        |> List.filterMap .color


toHexColor : Color -> String
toHexColor color =
    case color of
        Red ->
            red

        Green ->
            green

        Yellow ->
            yellow

        Blue ->
            blue


allColors : List Color
allColors =
    [ Red, Yellow, Green, Blue ]


unshuffledDeck : List Card
unshuffledDeck =
    let
        cardKinds : List CardKind
        cardKinds =
            List.concat
                [ allColors
                    |> List.concatMap createNumberCards
                , allColors |> List.map (toColorCard SkipCard)
                , allColors |> List.map (toColorCard ReverseCard)
                , allColors |> List.map (toColorCard DrawTwoCard)
                , List.repeat 4 WildCard
                , List.repeat 4 WildDraw4Card
                ]
    in
    cardKinds
        |> List.indexedMap
            (\index kind ->
                Card
                    { id = String.fromInt index
                    , kind = kind
                    }
            )


shuffle : Random.Seed -> List Card -> List Card
shuffle seed deck =
    let
        ( shuffledDeck, _ ) =
            Random.step (Random.List.shuffle deck) seed
    in
    shuffledDeck


createNumberCards : Color -> List CardKind
createNumberCards color =
    List.range 0 9
        |> List.map
            (\value ->
                NumberCard { value = value, color = color }
            )


toColorCard : ({ color : Color } -> CardKind) -> Color -> CardKind
toColorCard toCard color =
    toCard { color = color }



-- VIEW CODE


viewNumberCard : Id -> { color : String, value : Int } -> Svg msg
viewNumberCard id options =
    let
        shouldUnderlineNumber : Bool
        shouldUnderlineNumber =
            options.value == 6 || options.value == 9

        textDecoration : String
        textDecoration =
            if shouldUnderlineNumber then
                "text-decoration: underline"

            else
                "text-decoration: none"
    in
    Svg.svg
        [ Svg.Attributes.class "card"
        , Svg.Attributes.id id
        , Svg.Attributes.viewBox "0 0 5.5 9"
        , Svg.Attributes.width "200px"
        ]
        [ viewCardRectangleWithBackground options.color
        , viewCardSlantedEllipse

        -- big center number shadow
        , Svg.text_
            [ Svg.Attributes.class "card__big-number"
            , Svg.Attributes.x "50%"
            , Svg.Attributes.y "55%"
            , Svg.Attributes.fill black
            , Svg.Attributes.dominantBaseline "middle"
            , Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.style textDecoration
            ]
            [ Svg.text (String.fromInt options.value)
            ]

        -- big center number
        , Svg.text_
            [ Svg.Attributes.class "card__big-number"
            , Svg.Attributes.x "53%"
            , Svg.Attributes.y "53%"
            , Svg.Attributes.fill white
            , Svg.Attributes.dominantBaseline "middle"
            , Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.stroke black
            , Svg.Attributes.strokeWidth "0.1"
            , Svg.Attributes.style textDecoration
            ]
            [ Svg.text (String.fromInt options.value)
            ]

        -- top-left number shadow
        , Svg.text_
            [ Svg.Attributes.class "card__small-number"
            , Svg.Attributes.x "20%"
            , Svg.Attributes.y "17%"
            , Svg.Attributes.fill black
            , Svg.Attributes.dominantBaseline "middle"
            , Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.style textDecoration
            ]
            [ Svg.text (String.fromInt options.value)
            ]

        -- top-left number
        , Svg.text_
            [ Svg.Attributes.class "card__small-number"
            , Svg.Attributes.x "21%"
            , Svg.Attributes.y "16%"
            , Svg.Attributes.fill white
            , Svg.Attributes.dominantBaseline "middle"
            , Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.stroke black
            , Svg.Attributes.strokeWidth "0.05"
            , Svg.Attributes.style textDecoration
            ]
            [ Svg.text (String.fromInt options.value)
            ]

        -- bottom-right number shadow
        , Svg.text_
            [ Svg.Attributes.class "card__small-number"
            , Svg.Attributes.x "78%"
            , Svg.Attributes.y "86%"
            , Svg.Attributes.fill black
            , Svg.Attributes.dominantBaseline "middle"
            , Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.style textDecoration
            ]
            [ Svg.text (String.fromInt options.value)
            ]

        -- bottom-right number
        , Svg.text_
            [ Svg.Attributes.class "card__small-number"
            , Svg.Attributes.x "79%"
            , Svg.Attributes.y "85%"
            , Svg.Attributes.fill white
            , Svg.Attributes.dominantBaseline "middle"
            , Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.stroke black
            , Svg.Attributes.strokeWidth "0.05"
            , Svg.Attributes.style textDecoration
            ]
            [ Svg.text (String.fromInt options.value)
            ]
        ]


viewReverseCard : Id -> { color : String } -> Html msg
viewReverseCard id options =
    viewCardWithIcon
        { id = id
        , color = options.color
        , viewCenterIcon = viewReverseIcon
        , viewCornerIcon = viewReverseIcon
        }


viewSkipCard : Id -> { color : String } -> Html msg
viewSkipCard id options =
    viewCardWithIcon
        { id = id
        , color = options.color
        , viewCenterIcon = viewSkipIcon
        , viewCornerIcon = viewSkipIcon
        }


viewDrawTwoCard : Id -> { color : String } -> Html msg
viewDrawTwoCard id options =
    viewCardWithIcon
        { id = id
        , color = options.color
        , viewCenterIcon = viewTwoCardIcon options.color
        , viewCornerIcon = viewPlusNumberText 2
        }


viewWildCard : Id -> Html msg
viewWildCard id =
    viewCardWithIcon
        { id = id
        , color = black
        , viewCenterIcon = viewWildIcon
        , viewCornerIcon = viewSmallWildIcon
        }


viewWildDraw4Card : Id -> Html msg
viewWildDraw4Card id =
    viewCardWithIcon
        { id = id
        , color = black
        , viewCenterIcon =
            Svg.g []
                [ viewWildIcon
                , Svg.g [ Svg.Attributes.style "transform-origin: center; transform: rotate(9deg) skewY(-12deg) translate(0, -8%)" ] [ viewMiniCard green ]
                , Svg.g [ Svg.Attributes.style "transform-origin: center; transform: rotate(9deg) skewY(-12deg) translate(15%, 0)" ] [ viewMiniCard red ]
                , Svg.g [ Svg.Attributes.style "transform-origin: center; transform: rotate(9deg) skewY(-12deg) translate(-15%, 0)" ] [ viewMiniCard blue ]
                , Svg.g [ Svg.Attributes.style "transform-origin: center; transform: rotate(9deg) skewY(-12deg) translate(0, 8%)" ] [ viewMiniCard yellow ]
                ]
        , viewCornerIcon = viewPlusNumberText 4
        }



-- REUSABLE STUFF


viewCardWithIcon :
    { id : String
    , color : String
    , viewCenterIcon : Svg msg
    , viewCornerIcon : Svg msg
    }
    -> Html msg
viewCardWithIcon options =
    Svg.svg
        [ Svg.Attributes.class "card"
        , Svg.Attributes.id options.id
        , Svg.Attributes.viewBox "0 0 5.5 9"
        , Svg.Attributes.width "200px"
        ]
        [ viewCardRectangleWithBackground options.color
        , viewCardSlantedEllipse

        -- big center number shadow
        , options.viewCenterIcon
        , Svg.g
            [ Svg.Attributes.style "transform-origin: center; transform: translate(-28%, -35%) scale(0.35)"
            ]
            [ options.viewCornerIcon ]
        , Svg.g
            [ Svg.Attributes.style "transform-origin: center; transform: translate(28%, 35%) scale(0.35)"
            ]
            [ options.viewCornerIcon ]
        ]


viewReverseIcon : Svg msg
viewReverseIcon =
    let
        arrow : List (Svg msg)
        arrow =
            [ -- BLACK BACKGROUND
              Svg.polygon
                [ Svg.Attributes.points "2.4 2.9, 4.1 2.9, 3.6 4.325"
                , Svg.Attributes.fill black
                ]
                []
            , Svg.rect
                [ Svg.Attributes.x "1.9"
                , Svg.Attributes.y "3.35"
                , Svg.Attributes.width "0.85"
                , Svg.Attributes.height "1.75"
                , Svg.Attributes.fill black
                , Svg.Attributes.style "transform-origin: center; transform: rotate(45deg)"
                ]
                []

            -- WHITE FOREGROUND
            , Svg.polygon
                [ Svg.Attributes.points "2.65 3, 3.9 3, 3.6 4.125"
                , Svg.Attributes.fill white
                ]
                []
            , Svg.rect
                [ Svg.Attributes.x "2.0"
                , Svg.Attributes.y "3.45"
                , Svg.Attributes.width "0.65"
                , Svg.Attributes.height "1.5"
                , Svg.Attributes.fill white
                , Svg.Attributes.style "transform-origin: center; transform: rotate(45deg)"
                ]
                []
            ]
    in
    Svg.g []
        [ Svg.g [] arrow
        , Svg.g [ Svg.Attributes.style "transform-origin: center; transform: rotate(180deg)" ] arrow
        ]


viewSkipIcon : Svg msg
viewSkipIcon =
    Svg.g []
        [ Svg.circle
            [ Svg.Attributes.cx "47%"
            , Svg.Attributes.cy "52%"
            , Svg.Attributes.r "20%"
            , Svg.Attributes.fill "none"
            , Svg.Attributes.stroke black
            , Svg.Attributes.strokeWidth "0.5"
            ]
            []
        , Svg.circle
            [ Svg.Attributes.cx "50%"
            , Svg.Attributes.cy "50%"
            , Svg.Attributes.r "20%"
            , Svg.Attributes.fill "none"
            , Svg.Attributes.stroke black
            , Svg.Attributes.strokeWidth "0.75"
            ]
            []
        , Svg.line
            [ Svg.Attributes.x1 "63%"
            , Svg.Attributes.y1 "38%"
            , Svg.Attributes.x2 "26%"
            , Svg.Attributes.y2 "65%"
            , Svg.Attributes.stroke black
            , Svg.Attributes.strokeWidth "0.5"
            ]
            []
        , Svg.line
            [ Svg.Attributes.x1 "70%"
            , Svg.Attributes.y1 "38%"
            , Svg.Attributes.x2 "33%"
            , Svg.Attributes.y2 "65%"
            , Svg.Attributes.stroke black
            , Svg.Attributes.strokeWidth "0.5"
            ]
            []
        , Svg.circle
            [ Svg.Attributes.cx "50%"
            , Svg.Attributes.cy "50%"
            , Svg.Attributes.r "20%"
            , Svg.Attributes.fill "none"
            , Svg.Attributes.stroke white
            , Svg.Attributes.strokeWidth "0.5"
            ]
            []
        , Svg.line
            [ Svg.Attributes.x1 "69%"
            , Svg.Attributes.y1 "36%"
            , Svg.Attributes.x2 "32%"
            , Svg.Attributes.y2 "63%"
            , Svg.Attributes.stroke white
            , Svg.Attributes.strokeWidth "0.5"
            ]
            []
        ]


viewTwoCardIcon : String -> Svg msg
viewTwoCardIcon color =
    Svg.g []
        [ Svg.g [ Svg.Attributes.style "transform-origin: center; transform: rotate(9deg) skewY(-12deg) translate(5%, -5%)" ] [ viewMiniCard color ]
        , Svg.g [ Svg.Attributes.style "transform-origin: center; transform: rotate(9deg) skewY(-12deg) translate(-5%, 5%)" ] [ viewMiniCard color ]
        ]


viewSmallWildIcon : Svg msg
viewSmallWildIcon =
    Svg.g [ Svg.Attributes.style "transform-origin: center; transform: scale(0.65, 0.6)" ] [ viewWildIcon ]


viewWildIcon : Svg msg
viewWildIcon =
    Svg.g []
        [ Svg.mask [ Svg.Attributes.id "myMask" ]
            [ Svg.ellipse
                [ Svg.Attributes.fill white
                , Svg.Attributes.cx "50%"
                , Svg.Attributes.cy "50%"
                , Svg.Attributes.width "5"
                , Svg.Attributes.height "8.5"
                , Svg.Attributes.rx "30%"
                , Svg.Attributes.ry "40%"
                , Svg.Attributes.stroke black
                , Svg.Attributes.strokeWidth "0.35"
                , Svg.Attributes.style "transform: skewX(-23deg) translateX(35%); "
                ]
                []
            ]
        , Svg.ellipse
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.cx "50%"
            , Svg.Attributes.cy "50%"
            , Svg.Attributes.width "5"
            , Svg.Attributes.height "8.5"
            , Svg.Attributes.rx "30%"
            , Svg.Attributes.ry "40%"
            , Svg.Attributes.stroke white
            , Svg.Attributes.strokeWidth "0.35"
            , Svg.Attributes.style "transform: skewX(-23deg) translateX(35%); "
            ]
            []
        , Svg.g [ Svg.Attributes.mask "url(#myMask)" ]
            [ Svg.rect
                [ Svg.Attributes.x "0"
                , Svg.Attributes.y "0"
                , Svg.Attributes.width "50%"
                , Svg.Attributes.height "50%"
                , Svg.Attributes.fill red
                , Svg.Attributes.style "transform: skewX(-23deg) translateX(35%); "
                ]
                []
            , Svg.rect
                [ Svg.Attributes.x "50%"
                , Svg.Attributes.y "0"
                , Svg.Attributes.width "50%"
                , Svg.Attributes.height "50%"
                , Svg.Attributes.fill yellow
                , Svg.Attributes.style "transform: skewX(-23deg) translateX(35%); "
                ]
                []
            , Svg.rect
                [ Svg.Attributes.x "0"
                , Svg.Attributes.y "50%"
                , Svg.Attributes.width "50%"
                , Svg.Attributes.height "50%"
                , Svg.Attributes.fill green
                , Svg.Attributes.style "transform: skewX(-23deg) translateX(35%); "
                ]
                []
            , Svg.rect
                [ Svg.Attributes.x "50%"
                , Svg.Attributes.y "50%"
                , Svg.Attributes.width "50%"
                , Svg.Attributes.height "50%"
                , Svg.Attributes.fill blue
                , Svg.Attributes.style "transform: skewX(-23deg) translateX(35%); "
                ]
                []
            ]
        ]


viewMiniCard : String -> Svg msg
viewMiniCard color =
    Svg.g []
        [ Svg.rect
            [ Svg.Attributes.x "35%"
            , Svg.Attributes.y "38%"
            , Svg.Attributes.width "25%"
            , Svg.Attributes.height "26%"
            , Svg.Attributes.rx "5%"
            , Svg.Attributes.fill black
            , Svg.Attributes.stroke black
            , Svg.Attributes.strokeWidth "2%"
            ]
            []
        , Svg.rect
            [ Svg.Attributes.x "37.5%"
            , Svg.Attributes.y "37.5%"
            , Svg.Attributes.width "25%"
            , Svg.Attributes.height "25%"
            , Svg.Attributes.rx "5%"
            , Svg.Attributes.fill color
            , Svg.Attributes.stroke white
            , Svg.Attributes.strokeWidth "2%"
            ]
            []
        ]


viewPlusNumberText : Int -> Svg msg
viewPlusNumberText number =
    Svg.g []
        [ Svg.text_
            [ Svg.Attributes.class "card__plus-number"
            , Svg.Attributes.x "50%"
            , Svg.Attributes.y "50%"
            , Svg.Attributes.fill white
            , Svg.Attributes.dominantBaseline "middle"
            , Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.stroke black
            , Svg.Attributes.strokeWidth "0.1"
            ]
            [ Svg.text ("+" ++ String.fromInt number)
            ]
        ]



-- BUILDING BLOCKS FOR CARDS


viewCardRectangleWithBackground : String -> Svg msg
viewCardRectangleWithBackground bgColor =
    Svg.rect
        [ Svg.Attributes.fill bgColor
        , Svg.Attributes.x "0.25"
        , Svg.Attributes.y "0.25"
        , Svg.Attributes.width "5"
        , Svg.Attributes.height "8.5"
        , Svg.Attributes.rx "0.25"
        , Svg.Attributes.stroke white
        , Svg.Attributes.strokeWidth "0.25"
        ]
        []


viewCardSlantedEllipse : Svg msg
viewCardSlantedEllipse =
    Svg.ellipse
        [ Svg.Attributes.fill "none"
        , Svg.Attributes.cx "50%"
        , Svg.Attributes.cy "50%"
        , Svg.Attributes.width "5"
        , Svg.Attributes.height "8.5"
        , Svg.Attributes.rx "30%"
        , Svg.Attributes.ry "40%"
        , Svg.Attributes.stroke white
        , Svg.Attributes.strokeWidth "0.35"
        , Svg.Attributes.style "transform: skewX(-23deg) translateX(35%); "
        ]
        []


green : String
green =
    "#4f9b08"


red : String
red =
    "#c52138"


yellow : String
yellow =
    "#efd029"


blue : String
blue =
    "#0352a1"


white : String
white =
    "#FFFFFF"


black : String
black =
    "#000000"



-- FOR TESTING


blueSkipCard : Card
blueSkipCard =
    Card
        { id = "blueSkipCard"
        , kind = SkipCard { color = Blue }
        }


redDrawTwoCard : Card
redDrawTwoCard =
    Card
        { id = "redDrawTwoCard"
        , kind = DrawTwoCard { color = Red }
        }
