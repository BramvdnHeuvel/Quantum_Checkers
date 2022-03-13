module Game exposing ( BoardViewMode(..), GameView
                     , defaultGameView, selectPiece, resetGame, measureAt
                     )

import QBoard exposing (Measurement, QBoard, QPiece, startQBoard)
import Board  exposing (Player(..))

-- MODEL

type alias GameView =
    { turn     : Player
    , showMode : BoardViewMode
    , board    : QBoard
    }

type BoardViewMode
    = Idle
    | FromPerspectiveOf QPiece
    | FinishedGame

defaultGameView : GameView
defaultGameView =
    { turn     = White
    , showMode = Idle
    , board    = startQBoard
    }

-- UPDATE

measureAt : GameView -> Int -> Int -> Measurement -> (GameView, Cmd msg)
measureAt game _ _ _ =
    (game, Cmd.none) -- TODO: Execute a measurement on the board.

resetGame : (GameView, Cmd msg)
resetGame = (defaultGameView, Cmd.none)

selectPiece : GameView -> Int -> Int -> (GameView, Cmd msg)
selectPiece game x y =
    case game.showMode of
        
        -- Prepare a piece for making a move on the board,
        -- or ignore the event if clicked on nothing.
        Idle ->
            let
                piece : Maybe QPiece
                piece = QBoard.lookupSpot game.board x y
            in
                case piece of
                    Nothing ->
                        (game, Cmd.none)
                    
                    Just p ->
                        if p.owner /= game.turn then
                            (game, Cmd.none)
                        else
                            ( { game | showMode = FromPerspectiveOf p}
                            , Cmd.none
                            )
        
        -- The player clicked away to deactivate the option,
        -- or they clicked on a field to make a move.
        FromPerspectiveOf p ->
            -- TODO: Execute the proposed turn.
            let
                newBoard : QBoard
                newBoard = QBoard.moveQPiece game.board p x y
            in
            ( { game
              | board = newBoard
              , showMode = Idle
              }
            , QBoard.resolveCollision newBoard
            )
        
        -- If the game's finished,
        -- ignore all board interaction
        FinishedGame ->
            (game, Cmd.none)