module MainTest exposing (..)

import Card exposing (Card)
import Deck
import Dict
import Expect
import Main
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Main.elm"
        [ Test.test "my tests work" <|
            \_ ->
                Expect.equal (1 + 1) 2
        , Test.describe "Main.checkIfDraw2OrDraw4"
            [ Test.test "Does not change model when given a blue skip card" <|
                \_ ->
                    Main.checkIfDraw2OrDraw4 blueSkipCard initialModel
                        |> Expect.equal initialModel
            , Test.describe "When player plays Red Draw 2"
                [ Test.test "Computer 1's hand gets two cards" <|
                    \_ ->
                        let
                            modelAfterUpdating : Main.Model
                            modelAfterUpdating =
                                Main.checkIfDraw2OrDraw4 redDrawTwoCard initialModel

                            initialCardsInHand : Int
                            initialCardsInHand =
                                initialModel
                                    |> getSizeOfComputersHand 1

                            finalCardsInHand : Int
                            finalCardsInHand =
                                modelAfterUpdating
                                    |> getSizeOfComputersHand 1
                        in
                        Expect.equal
                            (initialCardsInHand + 2)
                            finalCardsInHand
                , Test.test "The deck loses 2 cards" <|
                    \_ ->
                        let
                            modelAfterUpdating : Main.Model
                            modelAfterUpdating =
                                Main.checkIfDraw2OrDraw4 redDrawTwoCard initialModel

                            initialCardsInDeck : Int
                            initialCardsInDeck =
                                Deck.size initialModel.deck

                            finalCardsInDeck : Int
                            finalCardsInDeck =
                                Deck.size modelAfterUpdating.deck
                        in
                        Expect.equal
                            (initialCardsInDeck - 2)
                            finalCardsInDeck
                , Test.test "Computer 2's hand does not change" <|
                    \_ ->
                        Main.checkIfDraw2OrDraw4 redDrawTwoCard initialModel
                            |> getSizeOfComputersHand 2
                            |> Expect.equal (initialModel |> getSizeOfComputersHand 2)
                , Test.test "Computer 3's hand does not change" <|
                    \_ ->
                        Main.checkIfDraw2OrDraw4 redDrawTwoCard initialModel
                            |> getSizeOfComputersHand 3
                            |> Expect.equal (initialModel |> getSizeOfComputersHand 3)
                ]
            ]
        , Test.describe "When computer 3 plays Red Draw 2"
            [ Test.test "Player's hand gets two cards" <|
                \_ ->
                    let
                        modelAfterUpdating : Main.Model
                        modelAfterUpdating =
                            Main.checkIfDraw2OrDraw4 redDrawTwoCard initialModelOnComputer3sTurn

                        initialCardsInHand : Int
                        initialCardsInHand =
                            initialModelOnComputer3sTurn
                                |> getSizeOfPlayersHand

                        finalCardsInHand : Int
                        finalCardsInHand =
                            modelAfterUpdating
                                |> getSizeOfPlayersHand
                    in
                    Expect.equal
                        (initialCardsInHand + 2)
                        finalCardsInHand
            , Test.test "The deck loses 2 cards" <|
                \_ ->
                    let
                        modelAfterUpdating : Main.Model
                        modelAfterUpdating =
                            Main.checkIfDraw2OrDraw4 redDrawTwoCard initialModelOnComputer3sTurn

                        initialCardsInDeck : Int
                        initialCardsInDeck =
                            Deck.size initialModelOnComputer3sTurn.deck

                        finalCardsInDeck : Int
                        finalCardsInDeck =
                            Deck.size modelAfterUpdating.deck
                    in
                    Expect.equal
                        (initialCardsInDeck - 2)
                        finalCardsInDeck
            , Test.test "Computer 2's hand does not change" <|
                \_ ->
                    Main.checkIfDraw2OrDraw4 redDrawTwoCard initialModelOnComputer3sTurn
                        |> getSizeOfComputersHand 2
                        |> Expect.equal (initialModelOnComputer3sTurn |> getSizeOfComputersHand 2)
            , Test.test "Computer 3's hand does not change" <|
                \_ ->
                    Main.checkIfDraw2OrDraw4 redDrawTwoCard initialModelOnComputer3sTurn
                        |> getSizeOfComputersHand 3
                        |> Expect.equal (initialModelOnComputer3sTurn |> getSizeOfComputersHand 3)
            ]
        ]


blueSkipCard : Card
blueSkipCard =
    Card.blueSkipCard


redDrawTwoCard : Card
redDrawTwoCard =
    Card.redDrawTwoCard


getSizeOfComputersHand : Int -> Main.Model -> Int
getSizeOfComputersHand int model =
    model.computerHands
        |> Dict.get int
        |> Maybe.withDefault []
        |> List.length


getSizeOfPlayersHand : Main.Model -> Int
getSizeOfPlayersHand model =
    model.playersHand
        |> List.length


initialModel : Main.Model
initialModel =
    Main.init { seed = 1 }
        |> Tuple.first


initialModelOnComputer3sTurn : Main.Model
initialModelOnComputer3sTurn =
    { initialModel | currentPlayerId = 3 }
