module Game exposing (GameView, defaultGameView)

import QBoard exposing (QBoard, startQBoard)
import Board  exposing (Piece, Player(..))

-- MODEL

type alias GameView =
    { turn     : Player
    , showMode : BoardViewMode
    , board    : QBoard
    }

type BoardViewMode
    = Idle
    | FromPerspectiveOf Piece

defaultGameView : GameView
defaultGameView =
    { turn     = White
    , showMode = Idle
    , board    = startQBoard
    }

-- UPDATE

