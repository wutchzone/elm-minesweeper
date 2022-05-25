module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model = {}


type TodoList
    = Loading
    | Error String
    | Success (List State)


type alias State = {}


type Msg = GameStart

init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
    , Cmd.none
    )

view : Model -> Html Msg
view model =
    Html.div [] [ Html.h1 [] [ Html.text "Hello there" ] ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameStart ->
            ( model, Cmd.none )
