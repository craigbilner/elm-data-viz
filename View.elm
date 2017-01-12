module View exposing (..)

import Html exposing (Html)
import Element exposing (toHtml)
import Axis exposing (calculateYAxis, calculateXAxis, xUnits, yUnits)
import Collage exposing (collage)
import Model exposing (Model)

view : Model -> Html msg
view model =
    let
        yAxis =
            calculateYAxis model

        xAxis =
            calculateXAxis model

        ( xTicks, xValues ) =
            xUnits model

        ( yTicks, yValues ) =
            yUnits model

        forms =
            List.concat
                [ xTicks
                , yTicks
                , xValues
                , yValues
                , [ xAxis
                  , yAxis
                  ]
                ]
    in
        collage
            (round model.width)
            (round model.height)
            forms
            |> toHtml
