module Layout exposing (showBoard, qubitBar)

import Html exposing   ( Html
                       , b, div, span, text, progress, h1
                       )
import Html.Attributes exposing (  style  , value )
import Html.Events     exposing ( onClick )

import Board   exposing ( Player(..), PieceSize(..) 
                        )
import QBoard  exposing ( QBoard, QPiece
                        , showPerspective, canQuantum
                        )
import Game    exposing ( GameView, BoardViewMode(..)
                        )
import Message exposing (Msg(..), Measurement
                        )
import QBoard exposing (Outcome(..))


-- MODEL

validFieldColor : String
validFieldColor = "darkgray"

nonValidFieldColor : String
nonValidFieldColor = "black"

showBoard : GameView -> Html Msg
showBoard game =
    let
        grid : String
        grid =
            let
                size : String
                size = String.fromInt Board.boardSize
            in
                "repeat(" ++ size ++ ", 1fr)"
            

        boardWrapper : List (Html Msg) -> Html Msg
        boardWrapper content = 
            div
                [ style "display" "grid"
                , style "grid-template" (grid ++ " / " ++ grid)
                , style "aspect-ratio" "1 / 1"
                , style "min-width" (String.fromInt (45 * Board.boardSize) ++ "px")
                , style "max-width" "75vh"
                , style "position" "relative"
                ] 
                content
        
        board : QBoard
        board =
            case game.showMode of
                Idle ->
                    game.board
                FromPerspectiveOf p ->
                    showPerspective game.board p
                FinishedGame _ ->
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
        
        canCapturePieces : List (Html Msg)
        canCapturePieces =
            if game.showMode == Idle then
                if QBoard.hasCaptureAvailable game.board game.turn then
                    QBoard.quantumView game.board
                        |> List.filter (QBoard.canMove game.board game.turn)
                        |> List.filter (\p -> p.owner == game.turn)
                        |> List.map (\p -> showOptionButton "orange" (p.x, p.y))
                else
                    []
            else
                []
        
        overlay : List (Html Msg)
        overlay =
            case game.showMode of
                FinishedGame outcome ->
                    case outcome of
                        Tie ->
                            gameOverlay
                            [ h1 [] [text "TIE"]
                            ]
                        
                        WonBy p ->
                            case p of
                                Black ->
                                    gameOverlay
                                    [ h1 [] [text "BLACK WINS"]
                                    ]
                                
                                White ->
                                    gameOverlay
                                    [ h1 [] [text "WHITE WINS"]
                                    ]
                
                _ ->
                    []
    in
        ( emptySquares 
        ++ pieces 
        ++ optionalMoves 
        ++ quantumMove 
        ++ canCapturePieces
        ++ overlay
        )
            |> boardWrapper

qubitBar : GameView -> Html Msg
qubitBar game =
    let
        qubits : Html.Attribute msg
        qubits = game.board
            |> List.length
            |> toFloat
            |> logBase 2
            |> String.fromFloat
            |> value
        
        maxQubits : Html.Attribute msg
        maxQubits = QBoard.maxQuantumBoards
            |> toFloat
            |> logBase 2
            |> String.fromFloat
            |> Html.Attributes.max
    in
        progress
            [ maxQubits
            , qubits
            , style "max-width" "75vh"
            ]
            []

-- UPDATE

gameOverlay : List (Html Msg) -> List (Html Msg)
gameOverlay content =
    let
        box : Html Msg
        box =
            div
                [ style "background-color" "gray"
                , style "border" "5px solid rgb(181, 154, 4)"
                , style "padding" "20px"
                , style "color" "rgb(255, 239, 153)"
                , style "min-width" "40%"
                , style "max-width" "80%"
                , style "text-align" "center"
                ]
                content
    in
        div
            [ style "position" "absolute"
            , style "top"    "0"
            , style "bottom" "0"
            , style "left"   "0"
            , style "right"  "0"
            , style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            ]
            [ box ]
            |> List.singleton

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


