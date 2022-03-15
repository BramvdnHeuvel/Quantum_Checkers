module QBoard exposing ( QBoard, QPiece, QuantumMoveResponse(..), Outcome(..)
                       , lookupSpot, moveQPiece, toNormalPiece, resolveCollision
                       , showPerspective, startQBoard, quantumView, canMove
                       , canMoveTo, makeQuantumMove, optimizeQBoard, canQuantum
                       , emptyAtSpot, showPerspectiveOfPiece, maxQuantumBoards
                       , hasCaptureAvailable, gameHasEnded
                       )

import Random

import Board exposing ( Board, Piece, Player, MoveResponse(..)
                      , startBoard
                      )
import Operations exposing (combineIncomparableValues, unique)
import Message exposing (Msg(..), Measurement)

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

type QuantumMoveResponse
    = SwitchTurn QBoard
    | RepeatTurn QBoard QPiece
    | InvalidMove

type Outcome
    = WonBy Player
    | Tie

startQBoard : QBoard
startQBoard = [ { board = startBoard
                , weight = 1
                }
              ]

-- UPDATE

gameHasEnded : QBoard -> Player -> Maybe Outcome
gameHasEnded qboard playerOnTurn =
    if canMoveAnywhere qboard playerOnTurn then
        Nothing
    else if hasPieceLeft qboard playerOnTurn then
        Just Tie
    else
        Just <| WonBy <| Board.nextTurn playerOnTurn

hasPieceLeft : QBoard -> Player -> Bool
hasPieceLeft qboard player =
    quantumView qboard
        |> List.filter (\p -> p.owner == player)
        |> List.isEmpty
        |> not

canMoveAnywhere : QBoard -> Player -> Bool
canMoveAnywhere qboard player =
    quantumView qboard
        |> List.filter (\p -> p.owner == player)
        |> List.any (canMove qboard player)

hasCaptureAvailable : QBoard -> Player -> Bool
hasCaptureAvailable qboard player =
    qBoardmap (\b -> Board.hasCaptureAvailable b player) qboard
        |> List.any (\x -> x)

canMove : QBoard -> Player -> QPiece -> Bool
canMove qboard player qpiece =
    canMoveTo qboard player qpiece
        |> List.isEmpty
        |> not

canMoveTo : QBoard -> Player -> QPiece -> List (Int, Int)
canMoveTo qboard player qpiece =
    let
        piece = toNormalPiece qpiece
    in
        if hasCaptureAvailable qboard player then
            showPerspective qboard qpiece
                |> qBoardmap (Board.canCaptureTo piece)
                |> List.concat
                |> unique
        else
            showPerspective qboard qpiece
                |> qBoardmap (Board.canWalkTo piece)
                |> List.concat
                |> unique

canQuantum : QBoard -> Player -> QPiece -> Bool
canQuantum qboard player qpiece =
    if hasCaptureAvailable qboard player then
        False
    else
        List.map .board qboard
            |> List.any (Board.canQuantum <| toNormalPiece qpiece)

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

emptyAtSpot : QBoard -> Int -> Int -> QBoard
emptyAtSpot qboard x y =
    let
        hasEmptySquare : Boardlet -> Bool
        hasEmptySquare bl =
            Board.lookUpSpot bl.board x y == Nothing
    in
        List.filter hasEmptySquare qboard

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

makeQuantumMove : QBoard -> Player -> QPiece -> QBoard
makeQuantumMove qboard player qpiece =
    let        
        processResponse : QuantumMoveResponse -> QBoard
        processResponse response =
            case response of
                InvalidMove ->
                    qboard
                
                RepeatTurn newqb _ ->
                    newqb
                
                SwitchTurn newqb ->
                    newqb
    in
        canMoveTo qboard player qpiece
            |> List.map (\(x, y) -> moveQPiece qboard player qpiece x y)
            |> List.map processResponse
            |> List.concat

moveQPiece : QBoard -> Player -> QPiece -> Int -> Int -> QuantumMoveResponse
moveQPiece qboard player qpiece x y =
    if qpiece.x == x && qpiece.y == y && canQuantum qboard player qpiece then
        makeQuantumMove qboard player qpiece
            |> optimizeQBoard
            |> SwitchTurn
    else
        let
            p : Piece
            p = toNormalPiece qpiece

            updateBoard : Board -> Board
            updateBoard board =
                case Board.movePiece board player p x y of
                    SuccessSameTurn b _ ->
                        b
                    SuccessNewTurn b ->
                        b
                    InvalidDestination ->
                        board
            
            hasSameTurn : Board -> Maybe Piece
            hasSameTurn board =
                case Board.movePiece board player p x y of
                    SuccessSameTurn _ newp ->
                        Just newp
                    _ ->
                        Nothing
            
            hasNewTurn : Board -> Bool
            hasNewTurn board =
                case Board.movePiece board player p x y of
                    SuccessNewTurn _ ->
                        True
                    _ ->
                        False
            
            chooseMessage : QBoard -> QBoard -> QuantumMoveResponse
            chooseMessage oldqb newqb =
                let
                    boards : List Board
                    boards = List.map .board oldqb
                in
                    case List.filterMap hasSameTurn boards of
                        [] ->
                            if List.any hasNewTurn boards then
                                SwitchTurn newqb
                            else
                                InvalidMove
                        
                        head :: _ ->
                            RepeatTurn newqb (toQPiece newqb head)

        in
            editBoards updateBoard qboard
                |> editBoards Board.upgradeReachedPieces
                |> optimizeQBoard
                |> chooseMessage qboard

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

        convToQPiece : (Piece, Int) -> QPiece
        convToQPiece (piece, count) =
            QPiece piece.owner piece.size piece.x piece.y (odds count)
        
        toWeightList : Boardlet -> List (Piece, Int)
        toWeightList boardlet =
            List.map (\b -> (b, boardlet.weight)) boardlet.board
    in
        qboard
            |> List.map toWeightList
            |> List.concat
            |> combineIncomparableValues
            |> List.map convToQPiece

spotCollision : QBoard -> List (Float, Measurement)
spotCollision qboard =
    let
        coords : List (Int, Int)
        coords =
            quantumView qboard
                |> List.map (\p -> (p.x, p.y))

        findMatch : List (Int, Int) -> Maybe (Int, Int)
        findMatch la =
            case la of
                [] ->
                    Nothing
                
                head :: body ->
                    case body of
                        [] ->
                            Nothing
                        
                        belly :: tail ->
                            if head == belly then
                                Just head
                            else
                                findMatch body
    in
        case findMatch (List.sort coords) of
            Nothing ->
                []
        
            Just (x, y) ->
                List.map
                    (\b -> (toFloat b.weight, Measurement x y (Board.lookUpSpot b.board x y))) 
                    qboard

createRandomMeasurement : List (Float, Measurement) -> Random.Generator Measurement
createRandomMeasurement la =
    case la of
        [] ->
            -- Should not happen
            Random.constant (Measurement 0 0 Nothing)

        head :: tail ->
            Random.weighted head tail


resolveCollision : QBoard -> Cmd Msg
resolveCollision qboard =
    let
        c : List (Float, Measurement)
        c = spotCollision qboard
    in
        if List.length c == 0 then
            Cmd.none
        else
            Random.generate Measure (createRandomMeasurement c)

showPerspectiveOfPiece : QBoard -> Piece -> QBoard
showPerspectiveOfPiece qboard piece =
    filterBoards (List.member piece) qboard

showPerspective : QBoard -> QPiece -> QBoard
showPerspective qboard qpiece =
    let
        p : Piece
        p = toNormalPiece qpiece
    in
        showPerspectiveOfPiece qboard p

toQPiece : QBoard -> Piece -> QPiece
toQPiece qboard piece =
    quantumView qboard
        |> List.filter (\p -> piece.x     == p.x    )
        |> List.filter (\p -> piece.y     == p.y    )
        |> List.filter (\p -> piece.owner == p.owner)
        |> List.filter (\p -> piece.size  == p.size )
        |> List.head
        |> Maybe.withDefault (QPiece Board.Black Board.Single 0 0 0)

toNormalPiece : QPiece -> Piece
toNormalPiece qp =
    { owner = qp.owner
    , size  = qp.size
    , x     = qp.x
    , y     = qp.y
    }

