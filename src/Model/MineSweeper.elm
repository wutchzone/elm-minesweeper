module Model.MineSweeper exposing (MineSweeperModel, Tile, getXSize, getYSize, newGame)

import Array


type alias MineSweeperModel =
    { x : Int, y : Int, numberOfMines : Int }


type alias Tile =
    { discovered : Bool, bombsAround : Int, flagged : Bool, isBomb : Bool }


{-| Instantiate instance of a new game.
-}
newGame : Int -> Int -> Int -> MineSweeperModel
newGame x y numberOfMines =
    { x = x, y = y, numberOfMines = numberOfMines }


{-| Get size of X axis.
-}
getXSize : MineSweeperModel -> Int
getXSize model =
    model.x


{-| Get size of Y axis.
-}
getYSize : MineSweeperModel -> Int
getYSize model =
    model.y


{-| Get tiles returns generated tiles of the model.
-}
getTiles : MineSweeperModel -> MineSweeperModel -> Array.Array (Array.Array Tile)
getTiles =
    Debug.todo "implement"


{-| Discovers individual tile on given coordinates.
Expects model and then returns modified model.
-}
discoverTile : MineSweeperModel -> MineSweeperModel -> Int -> Int
discoverTile =
    Debug.todo "implement"


{-| Flags individual tile if you think that there is a bomb on a given tile.
-}
flagTile : MineSweeperModel -> MineSweeperModel -> Int -> Int
flagTile =
    Debug.todo "implement"
