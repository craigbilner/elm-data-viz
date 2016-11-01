module Main exposing (..)

import Html.App as App
import Html exposing (Html)
import Element exposing (toHtml)
import Collage exposing (collage, Form, path, traced, solid)
import Color exposing (..)


type alias Model =
    { height : Float
    , width : Float
    , padding : Float
    , axisColour : Color.Color
    }


type Msg
    = Noop


initModel : Model
initModel =
    { height = 750
    , width = 1000
    , padding = 10
    , axisColour = black
    }


init : ( Model, Cmd msg )
init =
    ( initModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    ( model, Cmd.none )


calculateXAxis : Model -> Form
calculateXAxis { width, height, padding, axisColour } =
    let
        originX =
            ((-width / 2) + padding)

        originY =
            ((-height / 2) + padding)

        xStart =
            ( originX, originY )

        xEnd =
            ( width / 2 - padding, originY )
    in
        path [ xStart, xEnd ]
            |> traced (solid axisColour)


view : Model -> Html msg
view model =
    let
        xAxis =
            calculateXAxis model
    in
        collage
            (round model.width)
            (round model.height)
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
