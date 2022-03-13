module QBoard exposing ( Measurement, QBoard, QPiece
                       , lookupSpot, moveQPiece, toNormalPiece, resolveCollision
                       , showPerspective, startQBoard, quantumView, canMove
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
startQBoard = [startBoard, []]

-- UPDATE

canMove : QBoard -> QPiece -> Bool
canMove _ _ =
    True -- TODO: Determine whether the piece may move.

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

