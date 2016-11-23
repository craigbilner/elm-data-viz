port module Main exposing (..)

import Html.App as App
import Html exposing (Html)
import Date
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


type UnitType
    = Second
    | Minute
    | Hour
    | Day
    | Month
    | Year


timeDivisions : List ( UnitType, Float )
timeDivisions =
    [ ( Second, 1000 )
    , ( Minute, 60000 )
    , ( Hour, 3600000 )
    , ( Day, 86400000 )
    , ( Month, 2592000000 )
    , ( Year, 31104000000 )
    ]


findUnitType : Float -> List ( UnitType, Float ) -> ( UnitType, Float )
findUnitType value unitTypes =
    case unitTypes of
        [] ->
            ( Second, 1000 )

        [ x ] ->
            x

        x :: y :: xs ->
            if value < snd y then
                x
            else
                findUnitType value <| y :: xs


divUp : Float -> Float -> ( UnitType, Float )
divUp d n =
    let
        wholeUnit =
            findUnitType (d / n) timeDivisions

        unitCount =
            toFloat << ceiling <| d / n / snd wholeUnit
    in
        ( fst wholeUnit, snd wholeUnit * unitCount )


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


calculateZoomLevel : Float -> Float
calculateZoomLevel range =
    if range <= 1000 then
        0
    else if range <= 60000 then
        1000
    else if range <= 3600000 then
        60000
    else if range <= 86400000 then
        3600000
    else if range <= 2592000000 then
        86400000
    else
        2592000000


calculateZoom : Zoom -> Float -> Float -> Maybe ( Float, Float )
calculateZoom zoom min max =
    let
        zoomLevel =
            calculateZoomLevel <| max - min
    in
        case zoom of
            In ->
                Just ( min + zoomLevel, max - zoomLevel )

            Out ->
                Just ( min - zoomLevel, max + zoomLevel )


updateRange : Zoom -> Int -> Int -> Model -> Model
updateRange zoom x y model =
    let
        newRange =
            case model.range of
                Just ( min, max ) ->
                    calculateZoom zoom min max

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


makeValues : Float -> Float -> Float -> Float -> List Float
makeValues min max step remaining =
    if min >= max && remaining == 0 then
        min :: []
    else
        min :: (makeValues (min + step) max step (remaining - 1))


formatDate : UnitType -> Date.Date -> String
formatDate unitType date =
    case unitType of
        Second ->
            toString <| Date.second date

        Minute ->
            toString <| Date.minute date

        Hour ->
            toString <| Date.hour date

        Day ->
            (toString <| Date.day date) ++ " " ++ (toString <| Date.month date)

        Month ->
            (toString <| Date.month date) ++ " " ++ (toString <| Date.year date - 2000)

        Year ->
            toString <| Date.year date


makeXValue : UnitType -> Float -> Float -> Float -> Float -> Float -> Form
makeXValue unitType xOffset yOffset margin xPos xValue =
    let
        position =
            ( xPos - xOffset, -yOffset - margin )

        text =
            xValue
                |> Date.fromTime
                >> formatDate unitType
                |> fromString
                |> Collage.text
    in
        Collage.move position text


makeXTicks : Float -> Float -> Model -> List Float -> List Form
makeXTicks xOffset yOffset model ticks =
    List.map
        (makeXTick model.axisColour model.xTickHeight xOffset yOffset)
        ticks


makeXValues : Float -> Float -> Model -> List Float -> List Form
makeXValues xOffset yOffset model ticks =
    let
        ( min, max ) =
            minMax model.range model.data

        tickCount =
            toFloat (List.length ticks) - 1

        step =
            Debug.log "step" <| divUp (max - min) tickCount

        values =
            Debug.log "values" <| makeValues min max (snd step) tickCount
    in
        List.map2 (makeXValue (fst step) xOffset yOffset model.xTickValueMargin) ticks values


xUnits : Model -> ( List Form, List Form )
xUnits model =
    let
        ticks =
            [0..((model.width - model.padding * 2) / model.xTickSpread)]
                |> List.map (\x -> x * model.xTickSpread)

        xOffset =
            model.width / 2 - model.padding

        yOffset =
            model.height / 2 - model.padding
    in
        ( makeXTicks xOffset yOffset model ticks
        , makeXValues xOffset yOffset model ticks
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
