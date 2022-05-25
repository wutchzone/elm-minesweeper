module Model.MineSweeper exposing (MineSweeperModel, Tile, getXSize, getYSize, newGame)

import Array


type alias MineSweeperModel =
    { x : Int, y : Int, numberOfMines : Int }


type alias Tile =
    { discovered : Bool, bombsAround : Int, flagged : Bool, isBomb : Bool }


newGame : Int -> Int -> Int -> MineSweeperModel
newGame x y numberOfMines =
    { x = x, y = y, numberOfMines = numberOfMines }


getXSize : MineSweeperModel -> Int
getXSize model =
    model.x


getYSize : MineSweeperModel -> Int
getYSize model =
    model.y


getTiles : MineSweeperModel -> MineSweeperModel -> Array.Array (Array.Array Tile)
getTiles =
    Debug.todo "implement"


discoverTile : MineSweeperModel -> MineSweeperModel -> Int -> Int
discoverTile =
    Debug.todo "implement"


flagTile : MineSweeperModel -> MineSweeperModel -> Int -> Int
flagTile =
    Debug.todo "implement"
