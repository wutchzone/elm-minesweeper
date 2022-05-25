module Model.MineSweeper exposing (getXSize, getYSize, newGame)


type alias MineSweeperModel =
    { x : Int, y : Int, numberOfMines : Int }


newGame : Int -> Int -> Int -> MineSweeperModel
newGame x y numberOfMines =
    { x = x, y = y, numberOfMines = numberOfMines }


getXSize : MineSweeperModel -> Int
getXSize model =
    model.x


getYSize : MineSweeperModel -> Int
getYSize model =
    model.y
