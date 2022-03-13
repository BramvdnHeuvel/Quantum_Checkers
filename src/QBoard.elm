module QBoard exposing ( Measurement, QBoard, QPiece
                       , lookupSpot, moveQPiece, toNormalPiece, resolveCollision
                       , showPerspective, startQBoard, quantumView, canMove
                       , canMoveTo, makeQuantumMove, optimizeQBoard, canQuantum
                       )

import Board exposing ( Board, Piece, MoveResponse(..)
                      , startBoard
                      )
import Operations exposing (combineIncomparableValues, unique)

-- MODEL

maxQuantumBoards : Int
maxQuantumBoards = 256
-- Maximum amount of boards
-- This is to prevent the game from becoming too slow.

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

type QuantumMoveResponse
    = SwitchTurn QBoard
    | RepeatTurn QBoard QPiece
    | InvalidMove

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
            |> unique

canQuantum : QBoard -> QPiece -> Bool
canQuantum qboard qpiece =
    if List.length (canMoveTo qboard qpiece) >= 2 then
        List.length (makeQuantumMove qboard qpiece) <= maxQuantumBoards
    else
        False

reduceWeights : QBoard -> QBoard
reduceWeights qboard =
    let
        primes : List Int
        primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

        divBy : Int -> Boardlet -> Bool
        divBy p b =
            modBy p b.weight == 0
        
        commonDivisor : QBoard -> Int -> Maybe Int
        commonDivisor qb p =
            if List.all (divBy p) qb then
                Just p
            else
                Nothing

        goodPrimeNumbers : List Int
        goodPrimeNumbers = List.filterMap (commonDivisor qboard) primes

        weightDividedBy : Int -> Boardlet -> Boardlet
        weightDividedBy p boardlet =
            { boardlet | weight = boardlet.weight // p }
    in
        case goodPrimeNumbers of
            [] ->
                qboard
            
            head :: _ ->
                qboard
                    |> List.map (weightDividedBy head)
                    |> reduceWeights

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
            |> optimizeQBoard
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
                |> optimizeQBoard

optimizeQBoard : QBoard -> QBoard
optimizeQBoard qboard =
    let
        addState : Boardlet -> QBoard -> QBoard
        addState boardlet qb =
            if List.member boardlet.board (List.map .board qb) then
                List.map (addWeight boardlet) qb
            else
                qb ++ [boardlet]
        
        addWeight : Boardlet -> Boardlet -> Boardlet
        addWeight a b =
            if a.board == b.board then
                { b | weight = a.weight + b.weight }
            else
                b
    in
        List.foldr addState [] qboard
            |> reduceWeights

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
        total = List.foldr (\b t -> t + b.weight) 0 qboard 

        odds : Int -> Float
        odds v =
            100 * (toFloat v) / (toFloat total)

        toQPiece : (Piece, Int) -> QPiece
        toQPiece (piece, count) =
            QPiece piece.owner piece.size piece.x piece.y (odds count)
        
        toWeightList : Boardlet -> List (Piece, Int)
        toWeightList boardlet =
            List.map (\b -> (b, boardlet.weight)) boardlet.board
    in
        qboard
            |> List.map toWeightList
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

