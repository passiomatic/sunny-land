module Fx exposing
    ( Fx(..)
    , curtainClose
    , curtainDuration
    , curtainOpen
    , flash
    , flashDuration
    , interpolate
    , renderCurtain
    )

import Ease exposing (Easing)
import Math.Vector3 as Vec3 exposing (Vec3)
import Playground exposing (Color, Computer, Shape)


flashDuration =
    220


curtainDuration =
    550


type Fx
    = Flash Int
    | CurtainOpen Int
    | CurtainClose Int
    | None


flash =
    Flash flashDuration


curtainOpen =
    CurtainOpen curtainDuration


curtainClose =
    CurtainClose curtainDuration


renderCurtain : Color -> Computer -> Int -> List Shape
renderCurtain color computer timeout =
    let
        offset =
            Ease.inOutSine (toFloat timeout / curtainDuration) * computer.screen.width * 0.5
    in
    [ Playground.rectangle color (computer.screen.width * 0.5) computer.screen.height
        |> Playground.moveLeft (computer.screen.width * 0.25 + offset)
    , Playground.rectangle color (computer.screen.width * 0.5) computer.screen.height
        |> Playground.moveRight (computer.screen.width * 0.25 + offset)
    ]


{-| Interpolate from one color to another. See <https://stackoverflow.com/a/21010385>
-}
interpolate : Color -> Color -> Float -> Color
interpolate color1 color2 fraction =
    Vec3.sub color2 color1
        |> Vec3.scale fraction
        |> Vec3.add color1
