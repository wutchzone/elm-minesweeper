module MineSweeperTest exposing (testDiscoverTile, testInitializationOfModel)

import Array
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Model.MineSweeper exposing (..)
import Random exposing (Seed, int, step)
import Test exposing (..)


seed =
    Random.initialSeed 42


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
                        newGame x y mines seed
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
                        newGame x y mines seed
                in
                Expect.equal (getYSize game) y
        , test "test correct mine field size" <|
            \_ ->
                let
                    x =
                        10

                    y =
                        20

                    mines =
                        1

                    game =
                        newGame x y mines seed
                in
                Expect.equal (List.length (Array.toList (getTiles game))) (y * x)
        , test "validate mines and field calculation" <|
            \_ ->
                let
                    x =
                        3

                    y =
                        5

                    mines =
                        5

                    game =
                        newGame x y mines seed

                    expected =
                        Array.fromList
                            [ { bombsAround = 0, discovered = False, flagged = False, isBomb = True }
                            , { bombsAround = 2, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 1, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 1, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 3, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 1, discovered = False, flagged = False, isBomb = True }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 3, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 2, discovered = False, flagged = False, isBomb = True }
                            , { bombsAround = 1, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 3, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 2, discovered = False, flagged = False, isBomb = True }
                            , { bombsAround = 1, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 1, discovered = False, flagged = False, isBomb = True }
                            , { bombsAround = 2, discovered = False, flagged = False, isBomb = False }
                            ]
                in
                Expect.equal (getTiles game) expected
        ]


testDiscoverTile : Test
testDiscoverTile =
    describe
        "test discover tile 3x5"
        [ test "discover (0,0)" <|
            \_ ->
                let
                    x =
                        3

                    y =
                        5

                    mines =
                        0

                    game =
                        discoverTile (newGame x y mines seed) 0 0

                    expected =
                        Array.fromList
                            [ { bombsAround = 0, discovered = True, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            ]
                in
                Expect.equal (getTiles game) expected
        , test "discover (1,1)" <|
            \_ ->
                let
                    x =
                        3

                    y =
                        5

                    mines =
                        0

                    game =
                        discoverTile (newGame x y mines seed) 1 1

                    expected =
                        Array.fromList
                            [ { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = True, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            ]
                in
                Expect.equal (getTiles game) expected
        , test "discover (2,2)" <|
            \_ ->
                let
                    x =
                        3

                    y =
                        5

                    mines =
                        0

                    game =
                        discoverTile (newGame x y mines seed) 2 2

                    expected =
                        Array.fromList
                            [ { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = True, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            ]
                in
                Expect.equal (getTiles game) expected
        , test "discover (1,4)" <|
            \_ ->
                let
                    x =
                        3

                    y =
                        5

                    mines =
                        0

                    game =
                        discoverTile (newGame x y mines seed) 1 4

                    expected =
                        Array.fromList
                            [ { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = True, flagged = False, isBomb = False }
                            , { bombsAround = 0, discovered = False, flagged = False, isBomb = False }
                            ]
                in
                Expect.equal (getTiles game) expected
        ]
