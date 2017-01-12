port module Main exposing (..)

import Html.App as App
import Color exposing (black)
import Data exposing (lineData)
import Model exposing (..)
import View exposing (view)
import Update exposing (update)

initModel : Model
initModel =
    { height = 750
    , width = 1000
    , padding = 100
    , axisColour = black
    , xTickHeight = 10
    , yTickWidth = 10
    , xTickSpread = 50
    , yTickSpread = 50
    , xTickValueMargin = 20
    , xTickValueSpacing = 15
    , data = lineData
    , range = Nothing
    }

init : ( Model, Cmd msg )
init =
    ( initModel, Cmd.none )

port mouseWheel : (( Bool, Int, Int, Int ) -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
    mouseWheel Zooming

main : Program Never
main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
