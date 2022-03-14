module Message exposing (..)

import Board exposing (Piece)

type Msg
    = SelectPiece Int Int
    | ResetGame
    | Measure Measurement
    | FailedMeasure Int

type alias Measurement = 
    { x : Int
    , y : Int
    , piece : Maybe Piece
    }