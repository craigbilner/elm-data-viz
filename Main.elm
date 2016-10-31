module Main exposing (..)

import Html.App as App
import Html exposing (Html, div)


type alias Model =
    Int


type Msg
    = Noop


initModel : Model
initModel =
    1


init : ( Model, Cmd msg )
init =
    ( initModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html msg
view model =
    div [] []


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
