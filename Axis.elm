module Axis exposing (..)

import Date
import Color exposing (Color)
import Collage exposing (path, Form, traced, solid)
import Text exposing (fromString)
import Model exposing (..)
import Utils exposing (defaultMinMax)

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

minMax : Maybe ( Float, Float ) -> List ( Float, Float ) -> ( Float, Float )
minMax range data =
    case range of
        Just ( min, max ) ->
            ( min, max )

        _ ->
            defaultMinMax data

makeXTick : Color -> Float -> Float -> Float -> Float -> Form
makeXTick colour tickHeight xOffset yOffset xPos =
    path [ ( xPos - xOffset, -yOffset ), ( xPos - xOffset, -tickHeight - yOffset ) ]
        |> traced (solid colour)

makeXTicks : Float -> Float -> Model -> List Float -> List Form
makeXTicks xOffset yOffset model ticks =
    List.map
        (makeXTick model.axisColour model.xTickHeight xOffset yOffset)
        ticks

makeXText : Float -> Float -> Float -> Float -> String -> Form
makeXText xOffset margin xPos yOffset xValue =
    let
        position =
            ( xPos - xOffset, -yOffset - margin )

        text =
            xValue
                |> fromString
                |> Collage.text
    in
        Collage.move position text

makeValues : Float -> Float -> Float -> Float -> List Float
makeValues min max step remaining =
    if min >= max && remaining == 0 then
        min :: []
    else
        min :: (makeValues (min + step) max step (remaining - 1))

dateToTime : Date.Date -> String
dateToTime date =
    let
        hour =
            Date.hour date |> toString

        minute =
            Date.minute date |> toString

        second =
            Date.second date |> toString
    in
        hour ++ ":" ++ minute ++ ":" ++ second

makeXValue : UnitType -> Float -> Float -> Float -> Float -> Float -> Float -> List Form
makeXValue unitType xOffset yOffset margin spacing xPos xValue =
    let
        makeText =
            makeXText xOffset margin xPos

        date =
            Date.fromTime xValue
    in
        case unitType of
            Second ->
                [ makeText yOffset <| dateToTime date ]

            Minute ->
                [ makeText yOffset <| dateToTime date ]

            Hour ->
                [ makeText yOffset <|
                    (toString <| Date.day date)
                        ++ " "
                        ++ (toString <| Date.month date)
                , makeText (yOffset + spacing) <| dateToTime date
                ]

            Day ->
                [ makeText yOffset <|
                    (toString <| Date.day date)
                        ++ " "
                        ++ (toString <| Date.month date)
                , makeText (yOffset + spacing) <| toString <| Date.year date
                ]

            Month ->
                [ makeText yOffset <|
                    (toString <| Date.month date)
                        ++ " "
                        ++ (toString <| Date.year date - 2000)
                ]

            Year ->
                [ makeText yOffset <| toString <| Date.year date ]

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
        List.map2
            (makeXValue
                (fst step)
                xOffset
                yOffset
                model.xTickValueMargin
                model.xTickValueSpacing
            )
            ticks
            values
            |> List.concat

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

makeYTick : Color -> Float -> Float -> Float -> Float -> Form
makeYTick colour tickWidth xOffset yOffset yPos =
    path [ ( -xOffset, yPos - yOffset ), ( -tickWidth - xOffset, yPos - yOffset ) ]
        |> traced (solid colour)

makeYTicks : Float -> Float -> Model -> List Float -> List Form
makeYTicks xOffset yOffset model ticks =
    List.map
        (makeYTick model.axisColour model.yTickWidth xOffset yOffset)
        ticks

yUnits : Model -> ( List Form, List Form )
yUnits model =
    let
        ticks =
            [0..((model.height - model.padding * 2) / model.yTickSpread)]
                |> List.map (\x -> x * model.yTickSpread)

        xOffset =
            model.width / 2 - model.padding

        yOffset =
            model.height / 2 - model.padding
    in
        ( makeYTicks xOffset yOffset model ticks
        , []
        )

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

