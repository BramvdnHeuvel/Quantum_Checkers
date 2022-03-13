module Main exposing (main)

import Browser
import Html exposing (Html, div, p, text)

import Game exposing (GameView)
import Layout exposing (Msg(..), showBoard)

main : Program () Model Msg
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

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case (msg) of
        SelectPiece x y ->
            Game.selectPiece model x y
        
        ResetGame ->
            Game.resetGame
        
        Measure x y m ->
            Game.measureAt model x y m


-- SUBSCRIPTIONS

subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


-- VIEW

view : Model -> Html Msg
view model = div
    []
    [ showBoard model
    , p
        []
        [ text ((String.fromInt (List.length model.board)) ++ " quantum boards")
        ]
    ]


