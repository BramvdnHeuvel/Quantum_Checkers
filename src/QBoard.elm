module QBoard exposing (QBoard, QPiece, lookupSpot, moveQPiece, toNormalPiece, resolveCollision, startQBoard, Measurement, quantumView)

import Board exposing (Board, startBoard)
import Board exposing (Piece)

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
    qboard

quantumView : QBoard -> List QPiece
quantumView _ = [ -- This is mockup data.
    { owner = Board.Black
    , size  = Board.Single
    , x = 1
    , y = 1
    , odds = 100
    },
    
    { owner = Board.White
    , size  = Board.Single
    , x = 2
    , y = 2
    , odds = 100
    }]
-- TODO: Write function
-- This function converts the list of boards to
-- a single board with quantum pieces.

resolveCollision : QBoard -> Cmd msg
resolveCollision _ = Cmd.none -- TODO:
-- This function asks for a random measurement
-- when there's a conflict on the board.

toNormalPiece : QPiece -> Piece
toNormalPiece qp =
    { owner = qp.owner
    , size  = qp.size
    , x     = qp.x
    , y     = qp.y
    }

