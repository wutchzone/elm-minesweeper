module MineSweeperTest exposing (testInitializationOfModel)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Model.MineSweeper exposing (..)
import Test exposing (..)


testInitializationOfModel : Test
testInitializationOfModel =
    describe "test basic initialization of model 10x20"
        [ test "validate that x = 10" <|
            \_ ->
                let
                    x =
                        10

                    y =
                        20

                    mines =
                        1

                    game =
                        newGame x y mines
                in
                Expect.equal (getXSize game) x
        , test "validate that y = 20" <|
            \_ ->
                let
                    x =
                        10

                    y =
                        20

                    mines =
                        1

                    game =
                        newGame x y mines
                in
                Expect.equal (getYSize game) y
        ]
