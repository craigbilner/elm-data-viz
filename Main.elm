module Main exposing (..)

import Html.App as App
import Html exposing (Html)
import Element exposing (toHtml)
import Collage exposing (collage, Form, path, traced, solid)
import Text exposing (..)
import Color exposing (..)
import Data exposing (..)


type alias Model =
    { height : Float
    , width : Float
    , padding : Float
    , axisColour : Color.Color
    , xTickHeight : Float
    , xTickSpread : Float
    , xTickValueMargin : Float
    , data : List ( Float, Float )
    }


type Msg
    = Noop


initModel : Model
initModel =
    { height = 750
    , width = 1000
    , padding = 100
    , axisColour = black
    , xTickHeight = 10
    , xTickSpread = 50
    , xTickValueMargin = 20
    , data = lineData
    }


init : ( Model, Cmd msg )
init =
    ( initModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    ( model, Cmd.none )


calculateYAxis : Model -> Form
calculateYAxis { width, height, padding, axisColour } =
    let
        originX =
            ((-width / 2) + padding)

        originY =
            ((-height / 2) + padding)

        xStart =
            ( originX, originY )

        xEnd =
            ( originX, height / 2 - padding )
    in
        path [ xStart, xEnd ]
            |> traced (solid axisColour)


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


makeXTick : Color -> Float -> Float -> Float -> Float -> Form
makeXTick colour tickHeight xOffset yOffset xPos =
    path [ ( xPos - xOffset, -yOffset ), ( xPos - xOffset, -tickHeight - yOffset ) ]
        |> traced (solid colour)


minMax : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
minMax ( currentMin, currentMax ) ( xValue, _ ) =
    let
        newMin =
            if xValue < currentMin then
                xValue
            else
                currentMin

        newMax =
            if xValue > currentMax then
                xValue
            else
                currentMax
    in
        ( newMin, newMax )


makeValues : List Float -> List ( Float, Float ) -> List Float
makeValues units data =
    let
        ( min, max ) =
            List.foldl minMax ( 0, 0 ) data

        spread =
            (max - min) / (toFloat (List.length units))
    in
        List.map (\x -> toFloat (round (x * spread))) units


makeXValue : Float -> Float -> Float -> Float -> Float -> Form
makeXValue xOffset yOffset margin xPos xValue =
    Collage.move ( xPos - xOffset, -yOffset - margin ) (Collage.text (fromString (toString xValue)))


xUnits : Model -> ( List Form, List Form )
xUnits model =
    let
        units =
            [0..((model.width - model.padding * 2) / model.xTickSpread)]

        ticks =
            List.map (\x -> x * model.xTickSpread) units

        values =
            makeValues units model.data

        xOffset =
            model.width / 2 - model.padding

        yOffset =
            model.height / 2 - model.padding
    in
        ( List.map
            (makeXTick model.axisColour model.xTickHeight xOffset yOffset)
            ticks
        , List.map2 (makeXValue xOffset yOffset model.xTickValueMargin) ticks values
        )


view : Model -> Html msg
view model =
    let
        yAxis =
            calculateYAxis model

        xAxis =
            calculateXAxis model

        ( xTicks, xValues ) =
            xUnits model

        forms =
            List.concat
                [ xTicks
                , xValues
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
