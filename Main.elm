port module Main exposing (..)

import Html.App as App
import Html exposing (Html)
import Element exposing (toHtml)
import Collage exposing (collage, Form, path, traced, solid)
import Text exposing (..)
import Color exposing (..)
import List.Extra
import Data exposing (..)
import Debug


type alias Model =
    { height : Float
    , width : Float
    , padding : Float
    , axisColour : Color.Color
    , xTickHeight : Float
    , xTickSpread : Float
    , xTickValueMargin : Float
    , data : List ( Float, Float )
    , range : Maybe ( Float, Float )
    }


type Msg
    = Noop
    | Zooming ( Bool, Int, Int )


type Zoom
    = In
    | Out


divUp : Float -> Float -> Float
divUp d n =
    let
        result =
            toFloat << floor <| d / n
    in
        if (d / n) > result then
            result + 1
        else
            result


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
    , range = Nothing
    }


init : ( Model, Cmd msg )
init =
    ( initModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Zooming ( zoomingIn, x, y ) ->
            let
                zoom =
                    if zoomingIn then
                        In
                    else
                        Out
            in
                ( updateRange zoom x y model, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateRange : Zoom -> Int -> Int -> Model -> Model
updateRange zoom x y model =
    let
        newRange =
            case model.range of
                Just ( min, max ) ->
                    case zoom of
                        In ->
                            Just ( min + 1, max - 1 )

                        Out ->
                            Just ( min - 1, max + 1 )

                Nothing ->
                    Just <| defaultMinMax model.data
    in
        { model | range = newRange }


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


defaultMinMax : List ( Float, Float ) -> ( Float, Float )
defaultMinMax data =
    let
        min =
            case List.head data of
                Just ( a, _ ) ->
                    a

                _ ->
                    0

        max =
            case List.Extra.last data of
                Just ( a, _ ) ->
                    a

                _ ->
                    0
    in
        ( min, max )


minMax : Maybe ( Float, Float ) -> List ( Float, Float ) -> ( Float, Float )
minMax range data =
    case range of
        Just ( min, max ) ->
            ( min, max )

        _ ->
            defaultMinMax data


makeValues : Float -> Float -> Float -> List Float
makeValues min max step =
    if min >= max then
        min :: []
    else
        min :: (makeValues (min + step) max step)


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

        ( min, max ) =
            minMax model.range model.data

        tickCount =
            toFloat (List.length units) - 1

        step =
            divUp (max - min) tickCount

        stepMax =
            step * tickCount + min

        values =
            makeValues min stepMax step

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


port mouseWheel : (( Bool, Int, Int ) -> msg) -> Sub msg


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
