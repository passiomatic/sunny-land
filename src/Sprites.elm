module Sprites exposing
    ( cherry
    , creditsText
    , eagleIdle
    , enemyDie
    , forest
    , gem
    , gemEmpty
    , gemStill
    , heroFall
    , heroHit
    , heroIdle
    , heroJump
    , heroRun
    , instructionsText
    , itemFeedback
    , logo
    , opossumWalk
    , pressEnterText
    , sea
    , sky
    , spawnFrames
    , tileset, enemySpawn
    )

{-| Sprites for game scene and entities.
-}

import Direction exposing (Direction(..))
import Env
import Playground exposing (Shape, Time)
import Playground.Extra exposing (sprite)


tileset =
    Env.baseUrl ++ "/assets/environment/tileset.png"


entities =
    Env.baseUrl ++ "/assets/atlas/entities.png"


type alias SpriteAnimation =
    { frameDuration : Int
    , frameCount : Int
    , offset : ( Float, Float )
    , side : Direction -- Side the sprite is drawn
    }



-- ENVIRONMENT


sky =
    -- Repeat left and right to make animation easier
    let
        image =
            Playground.image 384 122 (Env.baseUrl ++ "/assets/environment/sky.png")
    in
    [ Playground.moveLeft 384 image
    , image
    , Playground.moveRight 384 image
    ]
        |> Playground.group
        |> Playground.moveUp 210


sea =
    Playground.image 384 138 (Env.baseUrl ++ "/assets/environment/sea.png")
        |> Playground.moveDown 180


forest =
    let
        image =
            Playground.image 176 368 (Env.baseUrl ++ "/assets/environment/forest.png")
    in
    [ image
        |> Playground.moveLeft 176
    , image
    , image
        |> Playground.moveRight 176
    ]
        |> Playground.group



-- TEXTS


logo =
    Playground.image 194 38 (Env.baseUrl ++ "/assets/titles/title-screen.png")


instructionsText =
    Playground.image 284 168 (Env.baseUrl ++ "/assets/titles/instructions.png")


pressEnterText =
    Playground.image 76 12 (Env.baseUrl ++ "/assets/titles/press-enter-text.png")


creditsText =
    Playground.image 186 16 (Env.baseUrl ++ "/assets/titles/credits-text.png")



-- HERO


heroRun : Direction -> Time -> Shape
heroRun dir time =
    spriteFrame dir
        time
        { frameDuration = 110
        , frameCount = 6
        , offset = ( 0, 3 )
        , side = East
        }
        heroRunFrames


heroIdle : Direction -> Time -> Shape
heroIdle dir time =
    spriteFrame dir
        time
        { frameDuration = 150
        , frameCount = 4
        , offset = ( 0, 3 )
        , side = East
        }
        heroIdleFrames


heroHit : Direction -> Time -> Shape
heroHit dir time =
    spriteFrame dir
        time
        { frameDuration = 75
        , frameCount = 2
        , offset = ( 0, 0 )
        , side = Neither
        }
        heroHitFrames


heroJump : Direction -> Shape
heroJump =
    heroJump_ 0


heroFall : Direction -> Shape
heroFall =
    heroJump_ 1


heroJump_ : Int -> Direction -> Shape
heroJump_ index dir =
    frame index heroJumpFrames
        |> Playground.move 0 3
        |> (case dir of
                West ->
                    Playground.flipX

                _ ->
                    identity
           )


heroHitFrames =
    ( sprite entities { xmin = 70, xmax = 102, ymin = 87, ymax = 118 }
    , [ sprite entities { xmin = 105, xmax = 137, ymin = 87, ymax = 118 }
      ]
    )


heroIdleFrames =
    ( sprite entities { xmin = 140, xmax = 172, ymin = 87, ymax = 118 }
    , [ sprite entities { xmin = 175, xmax = 207, ymin = 87, ymax = 118 }
      , sprite entities { xmin = 210, xmax = 242, ymin = 87, ymax = 118 }
      , sprite entities { xmin = 0, xmax = 32, ymin = 121, ymax = 152 }
      ]
    )


heroJumpFrames =
    ( sprite entities { xmin = 35, xmax = 67, ymin = 121, ymax = 152 }
    , [ sprite entities { xmin = 70, xmax = 102, ymin = 121, ymax = 152 }
      ]
    )


heroRunFrames =
    ( sprite entities { xmin = 105, xmax = 137, ymin = 121, ymax = 152 }
    , [ sprite entities { xmin = 140, xmax = 172, ymin = 121, ymax = 152 }
      , sprite entities { xmin = 175, xmax = 207, ymin = 121, ymax = 152 }
      , sprite entities { xmin = 210, xmax = 242, ymin = 121, ymax = 152 }
      , sprite entities { xmin = 0, xmax = 32, ymin = 155, ymax = 186 }
      , sprite entities { xmin = 35, xmax = 67, ymin = 155, ymax = 186 }
      ]
    )



-- EAGLE


eagleIdle : Direction -> Time -> Shape
eagleIdle dir time =
    spriteFrame dir
        time
        { frameDuration = 100
        , frameCount = 4
        , offset = ( -1, 2 )
        , side = West
        }
        eagleIdleFrames


eagleIdleFrames =
    ( sprite entities { xmin = 37, xmax = 76, ymin = 189, ymax = 229 }
    , [ sprite entities { xmin = 79, xmax = 118, ymin = 189, ymax = 229 }
      , sprite entities { xmin = 121, xmax = 160, ymin = 189, ymax = 229 }
      , sprite entities { xmin = 163, xmax = 202, ymin = 189, ymax = 229 }
      ]
    )


eagleDiveFrames =
    ( sprite entities { xmin = 205, xmax = 244, ymin = 189, ymax = 229 }
    , [ sprite entities { xmin = 0, xmax = 39, ymin = 232, ymax = 272 }
      ]
    )


eagleHurtFrames =
    ( sprite entities { xmin = 42, xmax = 81, ymin = 232, ymax = 272 }
    , [ sprite entities { xmin = 84, xmax = 123, ymin = 232, ymax = 272 }
      , sprite entities { xmin = 126, xmax = 165, ymin = 232, ymax = 272 }
      , sprite entities { xmin = 168, xmax = 207, ymin = 232, ymax = 272 }
      , sprite entities { xmin = 210, xmax = 249, ymin = 232, ymax = 272 }
      ]
    )



-- OPOSSUM


opossumWalk : Direction -> Time -> Shape
opossumWalk dir time =
    spriteFrame dir
        time
        { frameDuration = 140
        , frameCount = 6
        , offset = ( 0, -1 )
        , side = West
        }
        opossumWalkFrames


opossumWalkFrames =
    ( sprite entities { xmin = 23, xmax = 58, ymin = 23, ymax = 50 }
    , [ sprite entities { xmin = 61, xmax = 96, ymin = 23, ymax = 50 }
      , sprite entities { xmin = 99, xmax = 134, ymin = 23, ymax = 50 }
      , sprite entities { xmin = 137, xmax = 172, ymin = 23, ymax = 50 }
      , sprite entities { xmin = 175, xmax = 210, ymin = 23, ymax = 50 }
      , sprite entities { xmin = 213, xmax = 248, ymin = 23, ymax = 50 }
      ]
    )


enemyDie : Direction -> Time -> Shape
enemyDie dir time =
    spriteFrame dir
        time
        { frameDuration = 100
        , frameCount = 6
        , offset = ( 0, 0 )
        , side = Neither
        }
        deathFrames


deathFrames =
    ( sprite entities { xmin = 0, xmax = 39, ymin = 275, ymax = 315 }
    , [ sprite entities { xmin = 42, xmax = 81, ymin = 275, ymax = 315 }
      , sprite entities { xmin = 84, xmax = 123, ymin = 275, ymax = 315 }
      , sprite entities { xmin = 126, xmax = 165, ymin = 275, ymax = 315 }
      , sprite entities { xmin = 168, xmax = 207, ymin = 275, ymax = 315 }
      , sprite entities { xmin = 210, xmax = 249, ymin = 275, ymax = 315 }
      ]
    )


enemySpawn : Direction -> Time -> Shape
enemySpawn dir time =
    spriteFrame dir
        time
        { frameDuration = 100
        , frameCount = 6
        , offset = ( 0, 0 )
        , side = Neither
        }
        spawnFrames


spawnFrames =
    ( sprite entities { xmin = 210, xmax = 249, ymin = 275, ymax = 315 }
    , [ sprite entities { xmin = 168, xmax = 207, ymin = 275, ymax = 315 }
      , sprite entities { xmin = 126, xmax = 165, ymin = 275, ymax = 315 }
      , sprite entities { xmin = 84, xmax = 123, ymin = 275, ymax = 315 }
      , sprite entities { xmin = 42, xmax = 81, ymin = 275, ymax = 315 }
      , sprite entities { xmin = 0, xmax = 39, ymin = 275, ymax = 315 }
      ]
    )



-- ITEMS


gem : Direction -> Time -> Shape
gem dir time =
    spriteFrame dir
        time
        { frameDuration = 90
        , frameCount = 5
        , offset = ( 0, -1 )
        , side = Neither
        }
        gemFrames


gemEmpty =
    sprite entities { xmin = 85, xmax = 99, ymin = 0, ymax = 12 }
        |> Playground.move 0 -1


gemStill =
    sprite entities { xmin = 17, xmax = 31, ymin = 0, ymax = 12 }
        |> Playground.move 0 -1


gemFrames =
    ( sprite entities { xmin = 0, xmax = 14, ymin = 0, ymax = 12 }
    , [ sprite entities { xmin = 17, xmax = 31, ymin = 0, ymax = 12 }
      , sprite entities { xmin = 34, xmax = 48, ymin = 0, ymax = 12 }
      , sprite entities { xmin = 51, xmax = 65, ymin = 0, ymax = 12 }
      , sprite entities { xmin = 68, xmax = 82, ymin = 0, ymax = 12 }
      ]
    )


cherry : Direction -> Time -> Shape
cherry dir time =
    spriteFrame dir
        time
        { frameDuration = 90
        , frameCount = 7
        , offset = ( 0, 0 )
        , side = Neither
        }
        cherryFrames


cherryFrames =
    ( sprite entities { xmin = 102, xmax = 122, ymin = 0, ymax = 20 }
    , [ sprite entities { xmin = 125, xmax = 145, ymin = 0, ymax = 20 }
      , sprite entities { xmin = 148, xmax = 168, ymin = 0, ymax = 20 }
      , sprite entities { xmin = 171, xmax = 191, ymin = 0, ymax = 20 }
      , sprite entities { xmin = 194, xmax = 214, ymin = 0, ymax = 20 }
      , sprite entities { xmin = 217, xmax = 237, ymin = 0, ymax = 20 }
      , sprite entities { xmin = 0, xmax = 20, ymin = 23, ymax = 43 }
      ]
    )


itemFeedback : Direction -> Time -> Shape
itemFeedback dir time =
    spriteFrame dir
        time
        { frameDuration = 85
        , frameCount = 4
        , offset = ( 0, 0 )
        , side = Neither
        }
        itemFeedbackFrames


itemFeedbackFrames =
    ( sprite entities { xmin = 0, xmax = 31, ymin = 53, ymax = 84 }
    , [ sprite entities { xmin = 34, xmax = 65, ymin = 53, ymax = 84 }
      , sprite entities { xmin = 68, xmax = 99, ymin = 53, ymax = 84 }
      , sprite entities { xmin = 102, xmax = 133, ymin = 53, ymax = 84 }
      ]
    )



-- HELPERS


spriteFrame : Direction -> Time -> SpriteAnimation -> ( Shape, List Shape ) -> Shape
spriteFrame dir time anim frames =
    let
        ( x, y ) =
            anim.offset

        index =
            modBy anim.frameCount (time.now // anim.frameDuration)
    in
    frame index frames
        |> Playground.move x y
        |> (case ( dir, anim.side ) of
                ( West, East ) ->
                    Playground.flipX

                ( East, West ) ->
                    Playground.flipX

                _ ->
                    identity
           )


frame : Int -> ( a, List a ) -> a
frame index ( head, rest ) =
    case index of
        0 ->
            head

        _ ->
            List.drop (index - 1) rest
                |> List.head
                |> Maybe.withDefault head
