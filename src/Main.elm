module Main exposing (main)

import Browser
import Html exposing (Html, text)

import Game exposing (GameView, defaultGameView)

main = Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL

type alias Model = GameView

init : () -> (Model, Cmd msg)
init _ = Game.resetGame

-- UPDATE

type Msg
    = SelectPiece Int Int
    | ResetGame

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case (msg) of
        SelectPiece x y ->
            Game.selectPiece model x y
        
        ResetGame ->
            Game.resetGame


-- SUBSCRIPTIONS

subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


-- VIEW

view : Model -> Html msg
view model = text "Hello world!"


