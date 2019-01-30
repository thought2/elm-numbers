module Numbers exposing (linearSpace, reMap)

{-| Convenience functions for working with numbers

@docs linearSpace, reMap

-}


{-| Generate equally distanced values inside of a given range. With or without endpoint.
-}
linearSpace : ( Float, Float ) -> Int -> Bool -> List Float
linearSpace ( start, end ) n endpoint =
    let
        distance =
            end - start

        divisor =
            if endpoint then
                n - 1
            else
                n

        step =
            distance / toFloat divisor
    in
    List.range 0 (n - 1)
        |> List.map (toFloat >> (*) step >> (+) start)


{-| Remap a value from one range to another.
-}
reMap : ( Float, Float ) -> ( Float, Float ) -> Float -> Float
reMap range1 range2 value =
    let
        ( pivot1, _ ) =
            range1

        ( pivot2, _ ) =
            range2

        distance1 =
            directedDistance range1

        distance2 =
            directedDistance range2

        factor =
            (value - pivot1) / distance1
    in
    pivot2 + factor * distance2


directedDistance : ( Float, Float ) -> Float
directedDistance ( value1, value2 ) =
    value2 - value1
