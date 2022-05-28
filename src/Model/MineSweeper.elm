module Model.MineSweeper exposing
    ( GameStatus(..)
    , MineSweeperModel
    , Tile
    , decodeMineSweeperModel
    , discoverTile
    , encodeMineSweeperModel
    , getGameStatus
    , getPoints
    , getTiles
    , getXSize
    , getYSize
    , linearDiscoverTile
    , linearToggleFlagTile
    , newGame
    , toggleFlagTile
    )

import Array exposing (Array)
import Debug
import Html exposing (b)
import Json.Decode as Decode exposing (Decoder, bool, decodeString, float, int, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import List.Extra exposing (getAt, removeAt)
import Random exposing (Seed, int, step)


type GameStatus
    = GameRunning
    | GameOver
    | GameWon


type alias MineSweeperModel =
    { x : Int, y : Int, mines : Array Tile }


type alias Tile =
    { discovered : Bool, bombsAround : Int, flagged : Bool, isBomb : Bool }


encodeTile : Tile -> Encode.Value
encodeTile tile =
    Encode.object
        [ ( "discovered", Encode.bool tile.discovered )
        , ( "bombsAround", Encode.int tile.bombsAround )
        , ( "flagged", Encode.bool tile.flagged )
        , ( "isBomb", Encode.bool tile.isBomb )
        ]


decodeTile : Decoder Tile
decodeTile =
    Decode.succeed Tile
        |> required "discovered" bool
        |> required "bombsAround" Decode.int
        |> required "flagged" bool
        |> required "isBomb" bool


encodeMineSweeperModel : MineSweeperModel -> Encode.Value
encodeMineSweeperModel model =
    Encode.object
        [ ( "x", Encode.int model.x )
        , ( "y", Encode.int model.y )
        , ( "mines", Encode.array encodeTile model.mines )
        ]


decodeMineSweeperModel : Decoder MineSweeperModel
decodeMineSweeperModel =
    Decode.succeed MineSweeperModel
        |> required "x" Decode.int
        |> required "y" Decode.int
        |> required "mines" (Decode.array decodeTile)


makeTile : Bool -> Int -> Bool -> Bool -> Tile
makeTile discovered bombsAround flagged isBomb =
    Tile discovered bombsAround flagged isBomb


isBombOnIndex : Array Tile -> Int -> Int -> Int -> Int
isBombOnIndex mArr x y xSize =
    let
        index =
            y * xSize + x
    in
    if x >= 0 && y >= 0 then
        case Array.get index mArr of
            Just tile ->
                if tile.isBomb then
                    1

                else
                    0

            Nothing ->
                0

    else
        0


transformTile : Array Tile -> Int -> Int -> Tile -> Tile
transformTile shuffledMines xsize ind tile =
    let
        xindex =
            modBy xsize ind

        yindex =
            ind // xsize
    in
    { tile
        | bombsAround =
            isBombOnIndex shuffledMines (xindex + 1) yindex xsize
                + isBombOnIndex shuffledMines (xindex - 1) yindex xsize
                + isBombOnIndex shuffledMines xindex (yindex + 1) xsize
                + isBombOnIndex shuffledMines xindex (yindex - 1) xsize
                + isBombOnIndex shuffledMines (xindex + 1) (yindex + 1) xsize
                + isBombOnIndex shuffledMines (xindex + 1) (yindex - 1) xsize
                + isBombOnIndex shuffledMines (xindex - 1) (yindex + 1) xsize
                + isBombOnIndex shuffledMines (xindex - 1) (yindex - 1) xsize
    }


{-| Instantiate instance of a new game.
-}
newGame : Int -> Int -> Int -> Seed -> MineSweeperModel
newGame x y numberOfMines seed =
    let
        mines =
            Array.toList (Array.repeat numberOfMines (makeTile False 0 False True)) ++ Array.toList (Array.repeat (x * y - numberOfMines) (makeTile False 0 False False))

        shuffledMines =
            Array.fromList (shuffleList seed mines)

        calculatedMines =
            Array.indexedMap
                (transformTile shuffledMines x)
                shuffledMines
    in
    { x = x, y = y, mines = calculatedMines }


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
getTiles : MineSweeperModel -> Array.Array Tile
getTiles model =
    model.mines


isGameOver : MineSweeperModel -> Bool
isGameOver model =
    Array.foldl (||) False (Array.map (\item -> item.discovered && item.isBomb) model.mines)


isGameWin : MineSweeperModel -> Bool
isGameWin model =
    Array.foldl (&&) True (Array.map (\item -> item.flagged && item.isBomb || item.discovered && not item.isBomb) model.mines)


getGameStatus : MineSweeperModel -> GameStatus
getGameStatus model =
    if isGameOver model then
        GameOver

    else if isGameWin model then
        GameWon

    else
        GameRunning


getPoints : MineSweeperModel -> Int
getPoints model =
    Array.foldl (+)
        0
        (Array.map
            (\item ->
                if item.discovered && not item.isBomb then
                    1

                else
                    0
            )
            model.mines
        )


{-| Discovers individual tile on given coordinates.
Expects model and then returns modified model.
-}
discoverTile : MineSweeperModel -> Int -> Int -> MineSweeperModel
discoverTile prevModel x y =
    let
        index =
            y * getXSize prevModel + x
    in
    linearDiscoverTile prevModel index


{-| Same as discoverTile but the coordinations are expected as 1D array instead of a 2D
Coordinations
-}
linearDiscoverTile : MineSweeperModel -> Int -> MineSweeperModel
linearDiscoverTile prevModel index =
    case Array.get index prevModel.mines of
        Just tile ->
            { prevModel | mines = Array.set index { tile | discovered = True } prevModel.mines }

        Nothing ->
            prevModel


{-| Flags individual tile if you think that there is a bomb on a given tile.
-}
toggleFlagTile : MineSweeperModel -> Int -> Int -> MineSweeperModel
toggleFlagTile prevModel x y =
    let
        index =
            y * getXSize prevModel + x
    in
    linearToggleFlagTile prevModel index


{-| Same as flagTile but the coordinations are expected as 1D array instead of a 2D
Coordinations
-}
linearToggleFlagTile : MineSweeperModel -> Int -> MineSweeperModel
linearToggleFlagTile prevModel index =
    case Array.get index prevModel.mines of
        Just tile ->
            { prevModel | mines = Array.set index { tile | flagged = not tile.flagged } prevModel.mines }

        Nothing ->
            prevModel


{-| Stolen from: <https://stackoverflow.com/questions/42207900/how-to-shuffle-a-list-in-elm>
-}
shuffleList : Seed -> List a -> List a
shuffleList seed list =
    shuffleListHelper seed list []


shuffleListHelper : Seed -> List a -> List a -> List a
shuffleListHelper seed source result =
    if List.isEmpty source then
        result

    else
        let
            indexGenerator =
                Random.int 0 (List.length source - 1)

            ( index, nextSeed ) =
                step indexGenerator seed

            valAtIndex =
                getAt index source

            sourceWithoutIndex =
                removeAt index source
        in
        case valAtIndex of
            Just val ->
                shuffleListHelper nextSeed sourceWithoutIndex (val :: result)

            Nothing ->
                Debug.todo "generated an index outside list"
