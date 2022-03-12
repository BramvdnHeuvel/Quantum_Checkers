module Game exposing (GameView, defaultGameView, selectPiece)

import QBoard exposing (QBoard, QPiece, startQBoard)
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

defaultGameView : GameView
defaultGameView =
    { turn     = White
    , showMode = Idle
    , board    = startQBoard
    }

-- UPDATE

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
        FromPerspectiveOf _ ->
            -- TODO: Execute the proposed turn.
            (game, Cmd.none)