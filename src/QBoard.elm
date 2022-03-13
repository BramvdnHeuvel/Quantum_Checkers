module QBoard exposing ( Measurement, QBoard, QPiece
                       , lookupSpot, moveQPiece, toNormalPiece, resolveCollision
                       , showPerspective, startQBoard, quantumView
                       )

import Board exposing (Board, Piece, startBoard)
import Operations exposing (countIncomparableValues)

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
lookupSpot : QBoard -> Int -> Int -> Maybe QPiece
lookupSpot qboard x y =
    let
        pieces : List QPiece
        pieces = quantumView qboard
            |> List.filter (\p -> p.x == x)
            |> List.filter (\p -> p.y == y)
    in
        List.head pieces

moveQPiece : QBoard -> QPiece -> Int -> Int -> QBoard
moveQPiece qboard _ _ _ =
    qboard -- TODO: Write function for moving piece

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
            |> List.concat
            |> countIncomparableValues
            |> List.map toQPiece

resolveCollision : QBoard -> Cmd msg
resolveCollision _ = Cmd.none -- TODO:
-- This function asks for a random measurement
-- when there's a conflict on the board.

showPerspective : QBoard -> QPiece -> QBoard
showPerspective _ qpiece =
    let
        p : Piece
        p = toNormalPiece qpiece
    in
        []

toNormalPiece : QPiece -> Piece
toNormalPiece qp =
    { owner = qp.owner
    , size  = qp.size
    , x     = qp.x
    , y     = qp.y
    }

