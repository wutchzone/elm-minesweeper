port module Main exposing (main)

import Array
import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode exposing (decodeString)
import Json.Encode exposing (encode)
import Model.MineSweeper
    exposing
        ( GameStatus(..)
        , MineSweeperModel
        , Tile
        , decodeMineSweeperModel
        , encodeMineSweeperModel
        , getGameStatus
        , getPoints
        , getTiles
        , linearDiscoverTile
        , linearToggleFlagTile
        , newGame
        )
import Random exposing (Seed)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    load GameLoad


type alias Model =
    { minesweeper : MineSweeperModel
    }


type TodoList
    = Loading
    | Error String
    | Success (List State)


type alias State =
    {}


type Msg
    = GameRestart
    | DiscoverTile Tile Int
    | FlagTile Tile Int
    | GameLoad String


port save : String -> Cmd msg


port load : (String -> msg) -> Sub msg


port doload : () -> Cmd msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( { minesweeper = newGame 5 5 5 (Random.initialSeed 42)
      }
    , doload ()
    )


viewTileText : Tile -> String
viewTileText tile =
    if tile.isBomb && tile.discovered then
        "! BOMB !"

    else if tile.discovered then
        String.fromInt tile.bombsAround

    else if tile.flagged then
        "flag"

    else
        "not discovered"


viewTile : Bool -> Int -> Tile -> Html Msg
viewTile over index tile =
    Html.div
        ([ Attributes.class "grid-item"
         , Attributes.class
            (case tile.discovered of
                True ->
                    "discovered-tile"

                False ->
                    "hidden-tile"
            )
         ]
            ++ (case over of
                    False ->
                        [ Events.onDoubleClick <| DiscoverTile tile index
                        , Events.onClick <| FlagTile tile index
                        ]

                    True ->
                        []
               )
        )
        [ Html.text (viewTileText tile) ]


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.button [ Events.onClick <| GameRestart ] [ Html.text "Reset game" ]
        , Html.div [] [ Html.text ("Score: " ++ String.fromInt (getPoints model.minesweeper)) ]
        , Html.div []
            [ Html.text
                ("Game status: "
                    ++ (case getGameStatus model.minesweeper of
                            GameOver ->
                                "Exploded!"

                            GameWon ->
                                "Congratulations!"

                            GameRunning ->
                                "Running"
                       )
                )
            ]
        , Html.p [] [ Html.text "single click to flag, double click to discoved" ]
        , Html.div [ Attributes.class "grid-container" ]
            (List.indexedMap
                (viewTile
                    (case getGameStatus model.minesweeper of
                        GameOver ->
                            True

                        GameWon ->
                            True

                        GameRunning ->
                            False
                    )
                )
                (Array.toList
                    (getTiles
                        model.minesweeper
                    )
                )
            )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameRestart ->
            let
                newmodel =
                    { minesweeper = newGame 5 5 5 (Random.initialSeed 42)
                    }
            in
            ( newmodel
            , save ""
            )

        DiscoverTile tile index ->
            let
                modifiedmodel =
                    { model | minesweeper = linearDiscoverTile model.minesweeper index }
            in
            ( modifiedmodel, save (encode 0 (encodeMineSweeperModel modifiedmodel.minesweeper)) )

        FlagTile tile index ->
            let
                modifiedmodel =
                    { model | minesweeper = linearToggleFlagTile model.minesweeper index }
            in
            ( modifiedmodel, save (encode 0 (encodeMineSweeperModel modifiedmodel.minesweeper)) )

        GameLoad json ->
            ( case decodeString decodeMineSweeperModel json of
                Err _ ->
                    model

                -- well data are malformed or non-existent, so ignore them
                Ok parsedmodel ->
                    { model | minesweeper = parsedmodel }
            , Cmd.none
            )
