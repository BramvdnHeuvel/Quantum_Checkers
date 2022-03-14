module Board exposing (Board, Piece, Player(..), PieceSize(..), MoveResponse(..)
                      , boardSize, startBoard, canWalkTo, movePiece, canQuantum, canCaptureTo, canCaptureThere, hasCaptureAvailable
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

type MoveResponse
    = SuccessSameTurn Board Piece
    | SuccessNewTurn Board
    | InvalidDestination

startBoard : Board
startBoard = startPieces 4 -- ++ [Piece White Double 7 3]

-- UPDATE

hasCaptureAvailable : Board -> Bool
hasCaptureAvailable board =
    List.any (canCapture board) board

canCapture : Board -> Piece -> Bool
canCapture board piece =
    List.length (canCaptureTo piece board) > 0

canCaptureThere : Board -> Piece -> Int -> Int -> Bool
canCaptureThere board piece x y =
    List.member (x, y) <| canCaptureTo piece board

canCaptureTo : Piece -> Board -> List (Int, Int)
canCaptureTo piece board =
    let
        relCoords : Int -> Int -> (Int, Int)
        relCoords dx dy =
            (piece.x + dx, piece.y + dy)
        
        getRange : Int -> List (Int, Int)
        getRange r =
            [ relCoords (-1*r) (-1*r)
            , relCoords (-1*r) ( 1*r)
            , relCoords ( 1*r) (-1*r)
            , relCoords ( 1*r) ( 1*r)
            ]
        
        canSlayAPiece : (Int, Int) -> Bool
        canSlayAPiece (x, y) =
            let
                middlePieces : List Piece
                middlePieces = onPath board piece.x piece.y x y
            in
                if List.length middlePieces == 1 then
                    if lookUpSpot board x y == Nothing then
                        case middlePieces of
                            [] ->
                                False
                            head :: tail ->
                                head.owner /= piece.owner
                    else
                        False
                else
                    False
    in
        (if piece.size == Single then 2 else boardSize)
            |> List.range 2
            |> List.map getRange
            |> List.concat
            |> List.filter withinBounds
            |> List.filter canSlayAPiece

canWalkTo : Piece -> Board -> List (Int, Int)
canWalkTo piece board =
    let
        y_dir : Int
        y_dir =
            case piece.owner of
                Black -> -1
                White ->  1
        
        relCoords : Int -> Int -> (Int, Int)
        relCoords dx dy =
            (piece.x + dx, piece.y + dy)

        getRange : Int -> List (Int, Int)
        getRange r =
            if piece.size == Single then
                if r == 1 then
                    [relCoords 1 y_dir, relCoords -1 y_dir]
                else
                    []
            else
                [ relCoords (-1*r) (-1*r)
                , relCoords (-1*r) ( 1*r)
                , relCoords ( 1*r) (-1*r)
                , relCoords ( 1*r) ( 1*r)
                ]
            
    in
        if lookUpSpot board piece.x piece.y == Just piece then
            List.range 1 boardSize
                |> List.map getRange
                |> List.concat
                |> List.filter withinBounds
                |> List.filter (\(x, y) -> List.isEmpty (onPath board piece.x piece.y x y))
        else
            []

canWalkThere : Board -> Piece -> Int -> Int -> Bool
canWalkThere board piece x y =
    canWalkTo piece board
        |> List.member (x, y)

canQuantum : Piece -> Board -> Bool
canQuantum piece board =
    List.length (canWalkTo piece board) >= 2

lookUpSpot : Board -> Int -> Int -> Maybe Piece
lookUpSpot board x y =
    board
        |> List.filter (\p -> p.x == x)
        |> List.filter (\p -> p.y == y)
        |> List.head

movePiece : Board -> Piece -> Int -> Int -> MoveResponse
movePiece board piece x y =
    let
        selectMovedPiece : Piece -> Piece
        selectMovedPiece p =
            if p == piece then
                { p | x = x, y = y }
            else
                p
    in
        if hasCaptureAvailable board then
            -- A capture is possible
            InvalidDestination
        else
            -- Nothing to capture
            if not <| withinBounds (x, y) then
                InvalidDestination
            else if not <| canWalkThere board piece x y then
                InvalidDestination
            else 
                List.map selectMovedPiece board
                    |> SuccessNewTurn

onPath : Board -> Int -> Int -> Int -> Int -> List Piece
onPath board x1 y1 x2 y2 =
    let
        diff : Int
        diff = abs (x2 - x1)
    
        x_dir = (x2 - x1) // diff
        y_dir = (y2 - y1) // diff

        spots : List (Int, Int)
        spots =
            List.range 1 diff
                |> List.map (\r -> (x1 + r*x_dir, y1 + r*y_dir))
    in
        List.filterMap (\(x, y) -> lookUpSpot board x y) spots


startPieces : Int -> List Piece
startPieces rows =
    let
        fillRow : Player -> Int -> List Piece
        fillRow color y =
            List.range 1 boardSize
                |> List.filter (\x -> modBy 2 (x + y) == 0)
                |> List.map (\x -> Piece color Single  x y)

        fillRows : Int -> Int -> Player -> List Piece
        fillRows start end color =
            List.range start end
                |> List.map (fillRow color)
                |> List.concat
    in
        [fillRows 1 rows White, fillRows (1 + boardSize - rows) boardSize Black]
            |> List.concat

withinBounds : (Int, Int) -> Bool
withinBounds (x, y) =
    x >= 1 && x <= boardSize && y >= 1 && y <= boardSize