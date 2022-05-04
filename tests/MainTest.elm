module MainTest exposing (..)

import Card
import Deck
import Expect
import Fuzz
import Html.Attributes
import Main
import ProgramTest
import Random
import Test exposing (Test)
import Test.Html.Query
import Test.Html.Selector


type alias ProgramTest =
    ProgramTest.ProgramTest
        Main.Model
        Main.Msg
        (Cmd Main.Msg)


suite : Test
suite =
    Test.describe "Main"
        [ Test.test "Clicking the deck starts a new game, dealing 7 cards to the player" <|
            \_ ->
                initializeNewGame { seed = 1 }
                    |> playerClicksTheDeck
                    |> expectOurHandHasSevenCards
        , Test.fuzz positiveInt "Pile never has a wild card on top" <|
            \randomSeed ->
                initializeNewGame { seed = randomSeed }
                    |> playerClicksTheDeck
                    |> expectPileDoesNotHaveWildCardOnTop
        , Test.test "When a player plays their last card, they win the game" <|
            \_ ->
                initializeGameWith
                    { deck =
                        [ Card.NumberCard { value = 1, color = Card.Blue }
                        , Card.NumberCard { value = 1, color = Card.Blue }
                        , Card.NumberCard { value = 1, color = Card.Blue }
                        , Card.NumberCard { value = 1, color = Card.Blue }
                        , Card.NumberCard { value = 1, color = Card.Blue }
                        ]
                    , cardsToDraw = 1
                    }
                    |> playerPlaysCard "Blue 1"
                    |> expectToSeeYouWonMessage
        ]


positiveInt : Fuzz.Fuzzer Int
positiveInt =
    Fuzz.intRange 0 Random.maxInt


initializeNewGame : { seed : Int } -> ProgramTest
initializeNewGame { seed } =
    ProgramTest.createElement
        { init = Main.init
        , update = Main.update
        , view = Main.view
        }
        |> ProgramTest.start { seed = seed }


initializeGameWith : { deck : List Card.CardKind, cardsToDraw : Int } -> ProgramTest
initializeGameWith { deck, cardsToDraw } =
    ProgramTest.createElement
        { init =
            Main.initWithSettings
                { deck = Deck.fromKinds deck
                , cardsToDraw = cardsToDraw
                }
        , update = Main.update
        , view = Main.view
        }
        |> ProgramTest.start { seed = 1 }


playerClicksTheDeck : ProgramTest -> ProgramTest
playerClicksTheDeck programTest =
    programTest
        |> ProgramTest.clickButton "Draw a card"


playerPlaysCard : String -> ProgramTest -> ProgramTest
playerPlaysCard cardLabel programTest =
    programTest
        |> ProgramTest.within
            findPlayersHand
            (ProgramTest.clickButton cardLabel)


expectToSeeYouWonMessage : ProgramTest -> Expect.Expectation
expectToSeeYouWonMessage programTest =
    programTest
        |> ProgramTest.expectViewHas
            [ Test.Html.Selector.text "You won!"
            ]


expectOurHandHasSevenCards : ProgramTest -> Expect.Expectation
expectOurHandHasSevenCards programTest =
    programTest
        |> ProgramTest.expectView
            (\query ->
                query
                    |> findPlayersHand
                    |> findHandCardButtons
                    |> Test.Html.Query.count (Expect.equal 7)
            )


expectPileDoesNotHaveWildCardOnTop : ProgramTest -> Expect.Expectation
expectPileDoesNotHaveWildCardOnTop programTest =
    programTest
        |> ProgramTest.expectView
            (\query ->
                query
                    |> findPile
                    |> findTopCardInPile
                    |> Test.Html.Query.hasNot [ selectAriaLabel "Wild" ]
            )


selectAriaLabel : String -> Test.Html.Selector.Selector
selectAriaLabel value =
    Test.Html.Selector.attribute (Html.Attributes.attribute "aria-label" value)


findPile : Test.Html.Query.Single msg -> Test.Html.Query.Single msg
findPile query =
    query
        |> Test.Html.Query.find [ Test.Html.Selector.class "pile" ]


findTopCardInPile : Test.Html.Query.Single msg -> Test.Html.Query.Single msg
findTopCardInPile query =
    query
        |> Test.Html.Query.findAll [ Test.Html.Selector.class "card__wrapper" ]
        |> Test.Html.Query.index 0


findPlayersHand : Test.Html.Query.Single msg -> Test.Html.Query.Single msg
findPlayersHand query =
    query
        |> Test.Html.Query.find [ Test.Html.Selector.class "player-hand" ]


findHandCardButtons : Test.Html.Query.Single msg -> Test.Html.Query.Multiple msg
findHandCardButtons query =
    query
        |> Test.Html.Query.findAll [ Test.Html.Selector.class "hand__card-button" ]
