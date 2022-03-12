module Board exposing (Board, startBoard, Piece, Player(..), PieceSize)

-- MODEL

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
