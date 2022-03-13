module Board exposing (Board, Piece, Player(..), PieceSize(..)
                      , boardSize, startBoard
                      )

-- MODEL

boardSize : Int
boardSize = 10

type alias Board = List Piece

type alias Piece =
    { owner : Player
    , size  : PieceSize
    , x     : Int
    , y     : Int
    }

type Player
    = Black
    | White

type PieceSize
    = Single
    | Double

-- TODO: Create proper start board.
startBoard : Board
startBoard = []
