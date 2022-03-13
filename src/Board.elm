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
startBoard = startPieces

-- UPDATE

startPieces : List Piece
startPieces =
    let
        fillRow : Player -> Int -> List Piece
        fillRow color y =
            List.range 1 boardSize
                |> List.filter (\x -> modBy 2 (x + y) == 0)
                |> List.map (\x -> Piece color Single x y)

        fillRows : Int -> Int -> Player -> List Piece
        fillRows start end color =
            List.range start end
                |> List.map (fillRow color)
                |> List.concat
    in
        [fillRows 1 4 White, fillRows 7 10 Black]
            |> List.concat
