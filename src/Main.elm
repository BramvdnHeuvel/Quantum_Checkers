module Main exposing (main)

import Browser
import Html exposing (Html, text)

main = Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL

type alias Model =
    { turn : Int
    , board : List (List Int)
    }

init : () -> (Model, Cmd msg)
init _ =
    ( { turn = 1
      , board = []
      }
    , Cmd.none
    )

-- UPDATE

type Msg
    = Foo

update : Msg -> Model -> (Model, Cmd msg)
update msg model = 
    ( model
    , Cmd.none
    )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


-- VIEW

view : Model -> Html msg
view model = text "Hello world!"


