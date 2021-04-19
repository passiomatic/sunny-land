module Direction exposing
    ( Direction(..)
    , fromTuple
    , opposite
    )

import AltMath.Vector2 as Vec2 exposing (Vec2)


type Direction
    = North
    | East
    | South
    | West
    | Neither


opposite : Direction -> Direction
opposite dir =
    case dir of
        North ->
            South

        East ->
            West

        South ->
            North

        West ->
            East

        Neither ->
            Neither


fromTuple : ( Float, Float ) -> Direction
fromTuple ( x, y ) =
    if x > 0 then
        East

    else if x < 0 then
        West

    else if y > 0 then
        North

    else if y < 0 then
        South

    else
        Neither
