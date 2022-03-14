module Game exposing ( BoardViewMode(..), GameView
                     , defaultGameView, selectPiece, resetGame, measureAt
                     )

import QBoard exposing ( QBoard, QPiece
                       , startQBoard
                       )
import Board  exposing (Player(..))
import Message exposing (Msg, Measurement)

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

measureAt : GameView -> Measurement -> (GameView, Cmd msg)
measureAt game measure =
    case measure.piece of
        Nothing ->
            ( { game
              | board = QBoard.emptyAtSpot game.board measure.x measure.y
              }
            , Cmd.none
            )
        
        Just p ->
            ( { game
              | board = QBoard.showPerspectiveOfPiece game.board p
              }
            , Cmd.none
            )

resetGame : (GameView, Cmd msg)
resetGame = (defaultGameView, Cmd.none)

selectPiece : GameView -> Int -> Int -> (GameView, Cmd Msg)
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
                        else if not (QBoard.canMove game.board game.turn p) then
                            (game, Cmd.none)
                        else
                            ( { game | showMode = FromPerspectiveOf p}
                            , Cmd.none
                            )
        
        -- The player clicked away to deactivate the option,
        -- or they clicked on a field to make a move.
        FromPerspectiveOf p ->
            -- TODO: Execute the proposed turn.
            -- TODO: If the player may make an extra turn, already select
            --          that piece to make it clearer to the player.
            -- TODO: If the player selects another piece,
            --          switch to that piece's perspective. (The player may
            --          just wander between multiple pieces.)
            let
                newBoard : QBoard
                newBoard = QBoard.moveQPiece game.board game.turn p x y
                            |> QBoard.optimizeQBoard
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