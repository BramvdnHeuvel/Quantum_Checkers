module Game exposing ( BoardViewMode(..), GameView
                     , defaultGameView, selectPiece, resetGame, measureAt
                     )

import QBoard exposing ( QBoard, QPiece, QuantumMoveResponse(..), Outcome(..)
                       , startQBoard
                       )
import Board  exposing (Player(..))
import Message exposing (Msg, Measurement)

-- MODEL

type alias GameView =
    { turn      : Player
    , showMode  : BoardViewMode
    , board     : QBoard
    , onlyPiece : Maybe QPiece
    }

type BoardViewMode
    = Idle
    | FromPerspectiveOf QPiece
    | FinishedGame Outcome

defaultGameView : GameView
defaultGameView =
    { turn      = White
    , showMode  = Idle
    , board     = startQBoard
    , onlyPiece = Nothing
    }

-- UPDATE

measureAt : GameView -> Measurement -> (GameView, Cmd msg)
measureAt game measure =
    case measure.piece of
        Nothing ->
            ( { game
              | board = QBoard.emptyAtSpot game.board measure.x measure.y
              , showMode = Idle
              , onlyPiece = Nothing
              }
            , Cmd.none
            )
        
        Just p ->
            updateViewIfPieceDisappeared 
                ( { game
                | board = QBoard.showPerspectiveOfPiece game.board p
                }
                , Cmd.none
                )

updateViewIfPieceDisappeared : (GameView, Cmd msg) -> (GameView, Cmd msg)
updateViewIfPieceDisappeared (game, c) =
    case game.showMode of
        FromPerspectiveOf p ->
            if List.member p (QBoard.quantumView game.board) then
                (game, c)
            else
                ( { game
                  | showMode  = Idle
                  , onlyPiece = Nothing
                  }
                , c
                )
        
        _ ->
            (game, c)

resetGame : (GameView, Cmd msg)
resetGame = (defaultGameView, Cmd.none)

selectPiece : GameView -> Int -> Int -> (GameView, Cmd Msg)
selectPiece game x y =
    let
        maybeFinishGame : GameView -> GameView
        maybeFinishGame newgame =
            case QBoard.gameHasEnded newgame.board newgame.turn of
                Nothing ->
                    newgame
                
                Just o ->
                    { newgame | showMode = FinishedGame o }
    in
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
                case QBoard.moveQPiece game.board game.turn p x y of
                    InvalidMove ->
                        if game.onlyPiece == Nothing then
                            -- If the player selects another piece,
                            -- view from that piece's perspective.
                            selectPiece { game | showMode = Idle } x y
                        else
                            ( game
                            , Cmd.none
                            )
                    
                    SwitchTurn newBoard ->
                        ( { game
                        | turn      = Board.nextTurn game.turn
                        , board     = newBoard
                        , showMode  = Idle
                        , onlyPiece = Nothing
                        }
                            |> maybeFinishGame
                        , QBoard.resolveCollision newBoard
                        )
                    
                    RepeatTurn newBoard capturePiece ->
                        ( { game
                        | board     = newBoard
                        , showMode  = FromPerspectiveOf capturePiece
                        , onlyPiece = Just capturePiece
                        }
                            |> maybeFinishGame
                        , QBoard.resolveCollision newBoard
                        )
                
            
            -- If the game's finished,
            -- ignore all board interaction
            FinishedGame _ ->
                (game, Cmd.none)

