module Main exposing (main)

import Browser
import Card
import Html exposing (Html)
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
    { deck : List Card.Card
    , counter : Int
    }


init : Model
init =
    { deck = Card.unshuffledDeck
    , counter = 0
    }



-- UPDATE


type Msg
    = UserClickedShuffle


update : Msg -> Model -> Model
update msg model =
    case msg of
        UserClickedShuffle ->
            { model
                | deck =
                    model.deck
                        |> Card.shuffle (Random.initialSeed model.counter)
                , counter = model.counter + 1
            }



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.button
            [ Html.Events.onClick UserClickedShuffle ]
            [ Html.text "Shuffle!" ]
        , model.deck
            |> List.map Card.view
            |> Html.div []
        ]
