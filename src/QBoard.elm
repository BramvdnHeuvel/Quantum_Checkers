module QBoard exposing (QBoard, QPiece, startQBoard, lookupSpot)

import Board exposing (Board, startBoard)

-- MODEL

type alias QBoard = List Board

type alias QPiece =
    { owner : Board.Player
    , size  : Board.PieceSize
    , x     : Int
    , y     : Int
    , odds  : Float
    }

startQBoard : QBoard
startQBoard = [startBoard]

-- UPDATE

quantumView : QBoard -> List QPiece
quantumView _ = [] -- TODO: Write function
-- This function converts the list of boards to
-- a single board with quantum pieces.

lookupSpot : QBoard -> Int -> Int -> Maybe QPiece
lookupSpot qboard x y =
    let
        pieces : List QPiece
        pieces = quantumView qboard
            |> List.filter (\p -> p.x == x)
            |> List.filter (\p -> p.y == y)
    in
        List.head pieces

