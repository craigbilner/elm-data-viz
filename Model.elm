module Model exposing (..)

import Color exposing (Color)

type alias Model =
    { height : Float
    , width : Float
    , padding : Float
    , axisColour : Color.Color
    , xTickHeight : Float
    , yTickWidth : Float
    , xTickSpread : Float
    , yTickSpread : Float
    , xTickValueMargin : Float
    , xTickValueSpacing : Float
    , data : List ( Float, Float )
    , range : Maybe ( Float, Float )
    }


type Msg
    = Noop
    | Zooming ( Bool, Int, Int, Int )


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
