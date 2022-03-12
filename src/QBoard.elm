module QBoard exposing (..)

import Board exposing (Board, startBoard)

-- MODEL

type alias QBoard = List Board

startQBoard : QBoard
startQBoard = [startBoard]