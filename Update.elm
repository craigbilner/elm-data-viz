module Update exposing (..)

import Model exposing (..)
import Utils exposing (defaultMinMax)

calculateZoomSpeed : Int -> Float
calculateZoomSpeed delta =
    if delta < 10 then
        toFloat 1
    else if delta < 50 then
        toFloat 2
    else
        toFloat 3

calculateZoomLevel : Zoom -> Float -> Float
calculateZoomLevel zoom range =
    if range <= 1000 then
        if zoom == In then
            0
        else
            125
    else if range <= 60000 then
        125
    else if range <= 3600000 then
        7500
    else if range <= 86400000 then
        225000
    else if range <= 2592000000 then
        1375000
    else if range <= 5184000000 then
        21600000
    else if range <= 155520000000 then
        22000000
    else
        31104000000

calculateZoom : Zoom -> Float -> Float -> Float -> Maybe ( Float, Float )
calculateZoom zoom zoomSpeed min max =
    let
        zoomLevel =
            Debug.log "Zoom" <| calculateZoomLevel zoom (max - min)

        totalZoom =
            zoomLevel * zoomSpeed
    in
        case zoom of
            In ->
                Just ( min + totalZoom, max - totalZoom )

            Out ->
                Just ( min - totalZoom, max + totalZoom )

updateRange : Zoom -> Float -> Int -> Int -> Model -> Model
updateRange zoom zoomSpeed x y model =
    let
        newRange =
            case model.range of
                Just ( min, max ) ->
                    calculateZoom zoom zoomSpeed min max

                Nothing ->
                    Just <| defaultMinMax model.data
    in
        { model | range = newRange }

update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Zooming ( zoomingIn, x, y, delta ) ->
            let
                zoom =
                    if zoomingIn then
                        In
                    else
                        Out

                zoomSpeed =
                    calculateZoomSpeed delta
            in
                ( updateRange zoom zoomSpeed x y model, Cmd.none )

        _ ->
            ( model, Cmd.none )