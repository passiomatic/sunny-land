module Animation exposing
    ( alternate
    , slide
    )

{-| Additional animation functions for Playground rendering functions.
-}

import Playground exposing (Time)


{-| Go from low to high value and start again.
-}
slide lo hi period time =
    lo + (hi - lo) * toFrac period time


{-| Abruptly jump from low to high value periodically.
-}
alternate lo hi period time =
    if abs (2 * toFrac period time - 1) > 0.5 then
        hi

    else
        lo


{-| -}
toFrac : Float -> Time -> Float
toFrac period { now } =
    let
        ms =
            now

        p =
            period * 1000
    in
    toFloat (modBy (round p) ms) / p
