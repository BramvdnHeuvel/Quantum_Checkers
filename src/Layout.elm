module Layout exposing (Msg(..), showBoard)

import Html exposing   ( Html
                       , b, div, span, text
                       )
import Html.Attributes exposing (  style  )
import Html.Events     exposing ( onClick )

import Board exposing  ( Player(..), PieceSize(..) 
                       )
import QBoard exposing ( Measurement, QBoard, QPiece
                       , showPerspective, canQuantum
                       )
import Game exposing   ( GameView, BoardViewMode(..)
                       )


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
        
        board : QBoard
        board =
            case game.showMode of
                Idle ->
                    game.board
                FromPerspectiveOf p ->
                    showPerspective game.board p
                FinishedGame ->
                    game.board
                
        pieces : List (Html Msg)
        pieces = QBoard.quantumView board
            |> List.map showPiece
        
        optionalMoves : List (Html Msg)
        optionalMoves =
            case game.showMode of
                FromPerspectiveOf p ->
                    QBoard.canMoveTo board game.turn p
                        |> List.map (showOptionButton "green")
                _ ->
                    []
        
        quantumMove : List (Html Msg)
        quantumMove =
            case game.showMode of
                FromPerspectiveOf p ->
                    if canQuantum game.board game.turn p then
                        [showOptionButton "blue" (p.x, p.y)]
                    else
                        []
                _ ->
                    []
    in
        emptySquares ++ pieces ++ optionalMoves ++ quantumMove
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

showOptionButton : String -> (Int, Int) -> Html Msg
showOptionButton color (x, y) =
    div 
        [ style "grid-row"    (String.fromInt (1 + Board.boardSize - y))
        , style "grid-column" (String.fromInt x)
        , style "background-color" color
        , style "order"       "2"
        , onClick (SelectPiece x y)
        , style "opacity"     "50%"
        ]
        []

showSquare : Int -> Int -> String -> Int -> List (Html Msg) -> Html Msg
showSquare x y color order content =
    div 
        [ style "grid-row"    (String.fromInt (1 + Board.boardSize - y))
        , style "grid-column" (String.fromInt x)
        , style "background-color" color
        , style "order"       (String.fromInt order)
        , onClick (SelectPiece x y)
        ] 
        content

showCircle : QPiece -> Html msg
showCircle piece =
    let
        backgroundColor : String
        backgroundColor =
            case piece.owner of
                Black -> "Black"
                White -> "White"
        
        color : String
        color =
            case piece.owner of
                Black -> "white"
                White -> "black"
        
        shadow : List (Html.Attribute msg)
        shadow =
            if piece.size == Double then
                [style "box-shadow" ("3px 3px " ++ color)]
            else
                []

        -- Set the minimal opactiy at 10% to make small pieces
        -- not TOO invisible.
        odds : String
        odds = (String.fromFloat (max piece.odds 10)) ++ "%"

    in
        div
            (
            [ style "background-color" backgroundColor
            , style "width" "80%"
            , style "height" "80%"
            , style "border-radius" "100%"
            , style "margin" "10%"
            , style "opacity" odds
            , style "color" color
            , style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "font-size" "200%"
            , style "user-select" "none"
            ] ++ shadow
            )
            [ span 
                [] 
                [ b
                    []
                    [ text <| String.fromInt (round piece.odds)
                    ]
                ]
            ]


