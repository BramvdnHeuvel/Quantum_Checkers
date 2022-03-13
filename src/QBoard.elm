module QBoard exposing ( Measurement, QBoard, QPiece
                       , lookupSpot, moveQPiece, toNormalPiece, resolveCollision
                       , showPerspective, startQBoard, quantumView, canMove, canMoveTo, makeQuantumMove
                       )

import Board exposing (Board, Piece, MoveResponse(..), startBoard)
import Operations exposing (countIncomparableValues, combineIncomparableValues)
import Html exposing (p)

-- MODEL

type alias QBoard = List Boardlet

type alias Boardlet = { board  : Board
                      , weight : Int
                      }

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
startQBoard = [ { board = startBoard
                , weight = 1
                }
              ]

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
            |> qBoardmap (Board.canWalkTo piece)
            |> List.concat

canQuantum : QBoard -> QPiece -> Bool
canQuantum qboard qpiece =
    List.length (canMoveTo qboard qpiece) >= 2

filterBoards : (Board -> Bool) -> QBoard -> QBoard
filterBoards f qboard =
    let
        filter : Boardlet -> Bool
        filter b = f b.board
    in
        List.filter filter qboard

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
            editBoards updateBoard qboard

qBoardmap : (Board -> a) -> QBoard -> List a
qBoardmap f qboard =
    let
        alterBoard b = f b.board
    in
        List.map alterBoard qboard                

editBoards : (Board -> Board) -> QBoard -> QBoard
editBoards f qboard =
    let
        updateBoard : Boardlet -> Boardlet
        updateBoard b = { b | board = f b.board }
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
            |> qBoardmap countIncomparableValues  -- Optimize this
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
        filterBoards (List.member p) qboard

toNormalPiece : QPiece -> Piece
toNormalPiece qp =
    { owner = qp.owner
    , size  = qp.size
    , x     = qp.x
    , y     = qp.y
    }

