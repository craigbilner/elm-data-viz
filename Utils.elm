module Utils exposing (..)

import List.Extra

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
