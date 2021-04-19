module Vector2.Extra exposing
    ( toString
    , zero, rotateCounterclockwise
    )

import AltMath.Vector2 as Vec2 exposing (Vec2, vec2)
import Round


zero =
    vec2 0 0


{-| Rotate vector 90 degrees counterclockwise.
-}
rotateCounterclockwise : Vec2 -> Vec2
rotateCounterclockwise value =
    vec2 -value.y value.x


toString : Vec2 -> String
toString value =
    Round.round 2 value.x ++ ";" ++ Round.round 2 value.y
