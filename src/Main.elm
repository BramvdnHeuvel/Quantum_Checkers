module Main exposing (main)

import Browser
import Html exposing (Html, div, p, text)

import Game exposing (GameView)
import Message exposing (Msg(..))
import Html.Attributes exposing (id, style)
import Layout exposing (qubitBar, showBoard, turnBlock)

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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case (msg) of
        SelectPiece x y ->
            Game.selectPiece model x y
        
        ResetGame ->
            Game.resetGame
        
        Measure m ->
            Game.measureAt model m

        FailedMeasure _ ->
            ( model
            , Cmd.none
            )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


-- VIEW

view : Model -> Html Msg
view model = div
    [ id "checkers"
    , style "display" "flex"
    , style "flex-flow" "column nowrap"
    , style "align-items" "center"
    ]
    [ div
        []
        [ showBoard model
        , qubitBar  model
        , turnBlock model
        ]
    ]


