module Main exposing (main)

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes


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


main : Svg msg
main =
    Html.div []
        [ viewNumberCard { color = green, value = 9 }
        , viewNumberCard { color = red, value = 2 }
        , viewNumberCard { color = blue, value = 6 }
        , viewNumberCard { color = yellow, value = 4 }
        , viewReverseCard { color = blue }
        , viewReverseCard { color = green }
        , viewReverseCard { color = yellow }
        , viewReverseCard { color = red }
        , viewSkipCard { color = blue }
        , viewSkipCard { color = green }
        , viewSkipCard { color = yellow }
        , viewSkipCard { color = red }
        ]


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
        , viewIcon = viewReverseIcon
        }


viewSkipCard : { color : String } -> Html msg
viewSkipCard options =
    viewCardWithIcon
        { color = options.color
        , viewIcon = viewSkipIcon
        }



-- REUSABLE STUFF


viewCardWithIcon : { color : String, viewIcon : Svg msg } -> Html msg
viewCardWithIcon options =
    Svg.svg
        [ Svg.Attributes.viewBox "0 0 5.5 9"
        , Svg.Attributes.width "200px"
        ]
        [ viewCardRectangleWithBackground options.color
        , viewCardSlantedEllipse

        -- big center number shadow
        , options.viewIcon
        , Svg.g
            [ Svg.Attributes.style "transform-origin: center; transform: translate(-30%, -35%) scale(0.35)"
            ]
            [ options.viewIcon ]
        , Svg.g
            [ Svg.Attributes.style "transform-origin: center; transform: translate(30%, 35%) scale(0.35)"
            ]
            [ options.viewIcon ]
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
