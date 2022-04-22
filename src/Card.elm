module Card exposing (Card, Color, shuffle, unshuffledDeck, view, viewBackOfCard)

import Html exposing (Html)
import Random
import Random.List
import Svg exposing (Svg)
import Svg.Attributes


type Card
    = NumberCard { value : Int, color : Color }
    | ReverseCard { color : Color }
    | SkipCard { color : Color }
    | DrawTwoCard { color : Color }
    | WildCard
    | WildDraw4Card


view : Card -> Html msg
view card =
    case card of
        NumberCard { value, color } ->
            viewNumberCard
                { value = value
                , color = toHexColor color
                }

        ReverseCard { color } ->
            viewReverseCard
                { color = toHexColor color
                }

        SkipCard { color } ->
            viewSkipCard
                { color = toHexColor color
                }

        DrawTwoCard { color } ->
            viewDrawTwoCard
                { color = toHexColor color
                }

        WildCard ->
            viewWildCard

        WildDraw4Card ->
            viewWildDraw4Card


viewBackOfCard : Html msg
viewBackOfCard =
    viewCardWithIcon
        { color = black
        , viewCenterIcon = viewUnoCardBack
        , viewCornerIcon = Svg.g [] []
        }


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
    List.concat
        [ -- NUMBER CARDS
          allColors
            |> List.concatMap createNumberCards
        , allColors |> List.map (toColorCard SkipCard)
        , allColors |> List.map (toColorCard ReverseCard)
        , allColors |> List.map (toColorCard DrawTwoCard)
        , List.repeat 4 WildCard
        , List.repeat 4 WildDraw4Card
        ]


shuffle : Random.Seed -> List Card -> List Card
shuffle seed deck =
    let
        ( shuffledDeck, nextSeed ) =
            Random.step (Random.List.shuffle deck) seed
    in
    shuffledDeck


createNumberCards : Color -> List Card
createNumberCards color =
    List.range 0 9
        |> List.map
            (\value ->
                NumberCard { value = value, color = color }
            )


toColorCard : ({ color : Color } -> Card) -> Color -> Card
toColorCard toCard color =
    toCard { color = color }



-- VIEW CODE


viewNumberCard : { color : String, value : Int } -> Svg msg
viewNumberCard options =
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
        [ Svg.Attributes.viewBox "0 0 5.5 9"
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


viewReverseCard : { color : String } -> Html msg
viewReverseCard options =
    viewCardWithIcon
        { color = options.color
        , viewCenterIcon = viewReverseIcon
        , viewCornerIcon = viewReverseIcon
        }


viewSkipCard : { color : String } -> Html msg
viewSkipCard options =
    viewCardWithIcon
        { color = options.color
        , viewCenterIcon = viewSkipIcon
        , viewCornerIcon = viewSkipIcon
        }


viewDrawTwoCard : { color : String } -> Html msg
viewDrawTwoCard options =
    viewCardWithIcon
        { color = options.color
        , viewCenterIcon = viewTwoCardIcon options.color
        , viewCornerIcon = viewPlusNumberText 2
        }


viewWildCard : Html msg
viewWildCard =
    viewCardWithIcon
        { color = black
        , viewCenterIcon = viewWildIcon
        , viewCornerIcon = viewSmallWildIcon
        }


viewWildDraw4Card : Html msg
viewWildDraw4Card =
    viewCardWithIcon
        { color = black
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
    { color : String
    , viewCenterIcon : Svg msg
    , viewCornerIcon : Svg msg
    }
    -> Html msg
viewCardWithIcon options =
    Svg.svg
        [ Svg.Attributes.viewBox "0 0 5.5 9"
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
