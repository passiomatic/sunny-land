module Camera exposing
    ( Camera
    , follow
    , init
    )

import AltMath.Vector2 as Vec2 exposing (Vec2, vec2)
import Vector2.Extra as Vec2


type alias Camera =
    Vec2


minDistanceFromEdge =
    90


cameraSpeed =
    150


init : Vec2 -> Camera
init position =
    position


{-| Follow a given target if it goes beyond a "safe area" centered on screen.
-}
follow : { a | viewWidth : Float, viewHeight : Float, viewScale : Float } -> Float -> Vec2 -> Camera -> Camera
follow { viewWidth, viewHeight, viewScale } dt target camera =
    let
        newX =
            -- Check if target is moving torwards left/right edges
            if target.x < (camera.x - viewWidth * 0.5 / viewScale + minDistanceFromEdge) then
                target.x

            else if target.x > (camera.x + viewWidth * 0.5 / viewScale - minDistanceFromEdge) then
                target.x

            else
                camera.x

        newY =
            -- Check if target is moving torwards top/bottom edges
            if target.y > (camera.y + viewHeight * 0.5 / viewScale - minDistanceFromEdge) then
                target.y

            else if target.y < (camera.y - viewHeight * 0.5 / viewScale + minDistanceFromEdge) then
                target.y

            else
                camera.y

    in
    if newX /= camera.x || newY /= camera.y then
        -- Move camera to new position
        Vec2.direction (vec2 newX newY) camera
            |> Vec2.scale (dt * cameraSpeed)
            |> Vec2.add camera

    else
        camera
