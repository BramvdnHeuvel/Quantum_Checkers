module Layout exposing (showBoard, Msg(..))

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Board exposing (Player(..), PieceSize(..))
import QBoard exposing (Measurement, QPiece)
import Game exposing (GameView)


-- MODEL

validFieldColor : String
validFieldColor = "darkgray"

nonValidFieldColor : String
nonValidFieldColor = "black"

type Msg
    = SelectPiece Int Int
    | ResetGame
    | Measure Int Int Measurement

showBoard : GameView -> Html Msg
showBoard game =
    let
        boardWrapper : List (Html Msg) -> Html Msg
        boardWrapper content = 
            div
                [ style "display" "grid"
                , style "grid-template" "repeat(10, 1fr) / repeat(10, 1fr)"
                , style "aspect-ratio" "1 / 1"
                ] 
                content
        
        pieces : List (Html Msg)
        pieces = QBoard.quantumView game.board
            |> List.map showPiece
    in
        emptySquares ++ pieces -- TODO: Write pieces.
            |> boardWrapper

-- UPDATE

emptySquares : List (Html Msg)
emptySquares =
    let
        toTuple : Int -> Int -> (Int, Int)
        toTuple a b = (a, b)

        tupleList : Int -> List (Int, Int)
        tupleList x =
            List.range 1 Board.boardSize
                |> List.map (toTuple x)
        
        isOddSquare : (Int, Int) -> Bool
        isOddSquare (x, y) =
            modBy 2 (x + y) == 1
        
        createSquare : (Int, Int) -> Html Msg
        createSquare (x, y) =
            let
                color : String
                color =
                    if isOddSquare (x, y) then
                        nonValidFieldColor
                    else
                        validFieldColor
            in
            showSquare x y color 0 []
    in
        List.range 1 Board.boardSize
            |> List.map tupleList
            |> List.concat
            |> List.map createSquare

-- VIEW

showPiece : QPiece -> Html Msg
showPiece piece =
    showSquare piece.x piece.y validFieldColor 1 [showCircle piece]

showSquare : Int -> Int -> String -> Int -> List (Html Msg) -> Html Msg
showSquare x y color order content =
    div 
        [ style "grid-row"    (String.fromInt x)
        , style "grid-column" (String.fromInt y)
        , style "background-color" color
        , style "order"       (String.fromInt order)
        , onClick (SelectPiece x y)
        ] 
        content

showCircle : QPiece -> Html msg
showCircle piece =
    let
        color : String
        color =
            case piece.owner of
                Black -> "Black"
                White -> "White"
        
        odds : String
        odds = (String.fromFloat piece.odds) ++ "%"
    in
        div
            [ style "background-color" color
            , style "width" "80%"
            , style "height" "80%"
            , style "border-radius" "100%"
            , style "margin" "10%"
            , style "opacity" odds
            ]
            []


