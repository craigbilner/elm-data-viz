module Main exposing (..)

import Html.App as App
import Html exposing (Html)
import Element exposing (toHtml)
import Collage exposing (collage, path, traced, solid)
import Color exposing (red)


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
    let
        xAxis =
            path [ ( -400, -250 ), ( 400, -250 ) ] |> traced (solid red)
    in
        collage
            800
            500
            [ xAxis
            ]
            |> toHtml


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
