module QBoard exposing ( Measurement, QBoard, QPiece
                       , lookupSpot, moveQPiece, toNormalPiece, resolveCollision
                       , showPerspective, startQBoard, quantumView, canMove, canMoveTo, makeQuantumMove
                       )

import Board exposing (Board, Piece, MoveResponse(..), startBoard)
import Operations exposing (countIncomparableValues, combineIncomparableValues)
import Html exposing (p)

-- MODEL

type alias QBoard = List Board

type alias QPiece =
    { owner : Board.Player
    , size  : Board.PieceSize
    , x     : Int
    , y     : Int
    , odds  : Float
    }

type Measurement
    = Black
    | White
    | Empty

startQBoard : QBoard
startQBoard = [startBoard]

-- UPDATE

canMove : QBoard -> QPiece -> Bool
canMove qboard qpiece =
    canMoveTo qboard qpiece
        |> List.isEmpty
        |> not

canMoveTo : QBoard -> QPiece -> List (Int, Int)
canMoveTo qboard qpiece =
    let
        piece = toNormalPiece qpiece
    in
        showPerspective qboard qpiece
            |> List.map (Board.canWalkTo piece)
            |> List.concat

canQuantum : QBoard -> QPiece -> Bool
canQuantum qboard qpiece =
    List.length (canMoveTo qboard qpiece) >= 2

lookupSpot : QBoard -> Int -> Int -> Maybe QPiece
lookupSpot qboard x y =
    let
        pieces : List QPiece
        pieces = quantumView qboard
            |> List.filter (\p -> p.x == x)
            |> List.filter (\p -> p.y == y)
    in
        List.head pieces

makeQuantumMove : QBoard -> QPiece -> QBoard
makeQuantumMove qboard qpiece =
    canMoveTo qboard qpiece
        |> List.map (\(x, y) -> moveQPiece qboard qpiece x y)
        |> List.concat

moveQPiece : QBoard -> QPiece -> Int -> Int -> QBoard
moveQPiece qboard qpiece x y =
    if qpiece.x == x && qpiece.y == y && canQuantum qboard qpiece then
        makeQuantumMove qboard qpiece
    else
        let
            p : Piece
            p = toNormalPiece qpiece

            updateBoard : Board -> Board
            updateBoard board =
                case Board.movePiece board p x y of
                    SuccessSameTurn b _ ->
                        b
                    SuccessNewTurn b ->
                        b
                    InvalidDestination ->
                        board
        in
            List.map updateBoard qboard
                

quantumView : QBoard -> List QPiece
quantumView qboard =
    let 
        total : Int
        total = List.length qboard

        odds : Int -> Float
        odds v =
            100 * (toFloat v) / (toFloat total)

        toQPiece : (Piece, Int) -> QPiece
        toQPiece (piece, count) =
            QPiece piece.owner piece.size piece.x piece.y (odds count)
    in
        qboard
            |> List.map countIncomparableValues  -- Optimize this
            |> List.concat
            |> combineIncomparableValues
            |> List.map toQPiece

resolveCollision : QBoard -> Cmd msg
resolveCollision _ = Cmd.none -- TODO:
-- This function asks for a random measurement
-- when there's a conflict on the board.

showPerspective : QBoard -> QPiece -> QBoard
showPerspective qboard qpiece =
    let
        p : Piece
        p = toNormalPiece qpiece
    in
        List.filter (List.member p) qboard

toNormalPiece : QPiece -> Piece
toNormalPiece qp =
    { owner = qp.owner
    , size  = qp.size
    , x     = qp.x
    , y     = qp.y
    }

