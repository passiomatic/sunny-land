module Main exposing (main)

import AltMath.Vector2 as Vec2 exposing (Vec2, vec2)
import Animation
import Camera exposing (Camera)
import Diagnostic
import Dict exposing (Dict)
import Direction exposing (Direction(..))
import Ease exposing (Easing)
import Entity exposing (Entity, EntityType(..), Status(..), player)
import Fx exposing (Fx(..))
import Levels
import Physics exposing (Contact(..), Wall)
import Playground exposing (..)
import Playground.Extra
import Sprites
import Vector2.Extra as Vec2


{-| Scene global constants.
-}
config =
    { friction = vec2 -0.1 0 -- Friction with ground
    , g = vec2 0 -0.38 -- Gravity
    , viewScale = 3
    , viewWidth = 1000
    , viewHeight = 750
    , background = rgb 79 40 60
    }


totalGems =
    3


type alias Memory =
    { camera : Camera
    , entities : Dict Int Entity
    , collectedGems : Int
    , nextId : Int
    , score : Int
    , highScore : Int
    , debug : Bool
    , fx : Fx
    , notice : Notice
    , status : Status
    }


type alias Level =
    { data : String
    , spawns : List Spawn
    , walls : List Wall
    }


{-| Spawn point for a game entity.
-}
type alias Spawn =
    { p : Vec2
    , dir : Direction
    , type_ : EntityType
    }


type Status
    = Intro
    | Playing Level


type Notice
    = Notice String Int
    | Empty


init : Level -> Memory
init level =
    { camera = Camera.init Vec2.zero
    , entities = Dict.empty
    , collectedGems = 0
    , nextId = 100
    , score = 0
    , highScore = 10000
    , debug = False
    , fx = None
    , notice = Empty
    , status = Intro
    }


changeLevel : Level -> Memory -> Memory
changeLevel level memory =
    { memory
        | camera = findCameraPosition level.spawns
        , entities = Entity.fromSpawns level.spawns
        , collectedGems = 0
        , fx = Fx.curtainOpen
        , notice = Notice "Level 1. Ready!" 2000
        , status = Playing level
    }



-- VIEW


view : Computer -> Memory -> List Shape
view computer ({ debug, camera, entities } as memory) =
    let
        sky =
            -- Sky moves slower than camera
            Sprites.sky
                |> moveLeft (Animation.slide 0 (384 * config.viewScale) 50 computer.time)
                |> scale config.viewScale

        sea =
            Sprites.sea
                |> scale config.viewScale
    in
    case memory.status of
        Playing level ->
            let
                status =
                    renderStatus memory

                forest =
                    -- Forest moves slower than camera
                    Sprites.forest
                        |> moveDown 290
                        |> move (-camera.x * 0.7) (-camera.y * 0.7)
                        |> scale config.viewScale

                world =
                    renderWorld level
                        :: renderPhysicsGeometry debug level.walls entities
                        :: Entity.render computer.time entities
                        |> group
                        |> move (-camera.x * config.viewScale) -(camera.y * config.viewScale)
                        |> scale config.viewScale
            in
            sky
                :: sea
                :: forest
                :: world
                :: status
                :: renderMask computer memory
                :: renderFx computer memory
                :: renderNotice memory
                :: renderInventory memory
                :: []

        Intro ->
            let
                titles =
                    renderTitles computer
                        |> scale config.viewScale

                forest =
                    Sprites.forest
                        |> scale config.viewScale
                        |> moveDown 490
            in
            sky
                :: sea
                :: forest
                :: titles
                :: renderMask computer memory
                --:: status
                :: []


renderNotice : Memory -> Shape
renderNotice memory =
    case memory.notice of
        Notice text _ ->
            renderText yellow text
                |> moveUp 90

        Empty ->
            renderText yellow ""


{-| Draw a rectangular mask to "cutout" a portion of the world.
-}
renderMask : Computer -> Memory -> Shape
renderMask computer memory =
    let
        height =
            (computer.screen.height - config.viewHeight) / 2

        width =
            (computer.screen.width - config.viewWidth) / 2

        color =
            case memory.fx of
                Flash timeout ->
                    Ease.inCubic ((Fx.flashDuration - toFloat timeout) / Fx.flashDuration)
                        |> Fx.interpolate red config.background

                _ ->
                    config.background
    in
    -- Top
    (rectangle color computer.screen.width height
        |> moveY (computer.screen.top - height / 2)
    )
        -- Bottom
        :: (rectangle color computer.screen.width height
                |> moveY (computer.screen.bottom + height / 2)
           )
        -- Left
        :: (rectangle color width config.viewHeight
                |> moveX (computer.screen.left + width / 2)
           )
        -- Right
        :: (rectangle color width config.viewHeight
                |> moveX (computer.screen.right - width / 2)
           )
        :: []
        |> group


{-| Draw a game status header.
-}
renderStatus : Memory -> Shape
renderStatus memory =
    (case Entity.player memory.entities of
        Just player ->
            [ -- First column
              renderHealth (max 0 player.health)
                |> moveLeft (config.viewWidth * 0.5 - 100)

            -- Second column
            , renderText white ("Score " ++ String.padLeft 5 '0' (String.fromInt memory.score))

            -- Third column
            , renderText yellow ("High " ++ String.padLeft 5 '0' (String.fromInt (max memory.score memory.highScore)))
                |> moveRight (config.viewWidth * 0.5 - 135)
            ]

        --|> Diagnostic.consIf memory.debug (Diagnostic.entity player)
        Nothing ->
            []
    )
        |> group
        |> moveUp (config.viewHeight * 0.5 - 30)


renderInventory memory =
    List.range 1 totalGems
        |> List.map
            (\index ->
                (if memory.collectedGems >= index then
                    Sprites.gemStill

                 else
                    Sprites.gemEmpty
                )
                    |> moveRight (toFloat index * 14)
            )
        |> group
        |> moveLeft (config.viewWidth * 0.5 - 15)
        |> moveDown (config.viewHeight * 0.5 - 40)
        |> scale config.viewScale


{-| Draw a line of text with a solid shadow underneath.
-}
renderText : Color -> String -> Shape
renderText color value =
    (words black value |> move 2 -2)
        :: words color value
        :: []
        |> group


{-| Draw a health gauge.
-}
renderHealth : Float -> Shape
renderHealth value =
    let
        color =
            if value > 0.4 then
                blue

            else if value > 0.2 && value <= 0.4 then
                yellow

            else
                red

        width =
            value * 120

        gauge =
            rectangle color width 10
                :: (rectangle white width 2
                        |> moveUp 2
                   )
                :: []
                |> group
                |> moveLeft ((120 - width) * 0.5)
    in
    rectangle darkBlue 126 16
        :: gauge
        :: []
        |> group


renderWorld : Level -> Shape
renderWorld level =
    Playground.Extra.tilemap 16 16 Sprites.tileset level.data


renderTitles computer =
    [ Sprites.logo
        |> moveUp 40
    , Sprites.instructionsText
        |> moveDown 45
    , Sprites.pressEnterText
        |> moveDown 115
        |> fade (Animation.alternate 0 1 1.5 computer.time)

    -- , Sprites.creditsText
    --     |> moveDown 142
    ]
        |> group
        |> moveUp 120


renderPhysicsGeometry debug walls entities =
    (if debug then
        let
            walls_ =
                List.map Diagnostic.wall walls

            entities_ =
                Dict.map (\_ entity -> Diagnostic.body entity) entities
                    |> Dict.values
        in
        List.append entities_ walls_

     else
        []
    )
        |> group
        |> fade 0.8


renderFx : Computer -> { a | fx : Fx } -> Shape
renderFx computer memory =
    (case memory.fx of
        CurtainOpen timeout ->
            Fx.renderCurtain config.background computer (Fx.curtainDuration - timeout)

        CurtainClose timeout ->
            Fx.renderCurtain config.background computer timeout

        Flash _ ->
            []

        None ->
            []
    )
        |> group



-- UPDATE


update : Computer -> Memory -> Memory
update computer memory =
    let
        dt =
            toFloat computer.time.delta / 1000
    in
    case memory.status of
        Playing level ->
            memory
                |> Entity.update computer config
                |> simulate dt level.walls
                |> updateCamera dt
                |> updateFx computer
                |> updateNotice computer

        Intro ->
            if computer.keyboard.enter then
                -- Start game
                changeLevel Levels.level1 memory

            else
                memory


updateFx : Computer -> Memory -> Memory
updateFx computer memory =
    { memory
        | fx =
            case memory.fx of
                CurtainOpen timeout ->
                    updateFxHelp CurtainOpen computer timeout

                CurtainClose timeout ->
                    updateFxHelp CurtainClose computer timeout

                Flash timeout ->
                    updateFxHelp Flash computer timeout

                None ->
                    None
    }


updateFxHelp : (Int -> Fx) -> Computer -> Int -> Fx
updateFxHelp tagger computer timeout =
    let
        remaining =
            timeout - computer.time.delta
    in
    if remaining > 0 then
        tagger remaining

    else
        None


updateNotice : Computer -> Memory -> Memory
updateNotice computer memory =
    { memory
        | notice =
            case memory.notice of
                Notice text timeout ->
                    let
                        remaining =
                            timeout - computer.time.delta
                    in
                    if remaining > 0 then
                        Notice text remaining

                    else
                        Empty

                Empty ->
                    Empty
    }


{-| Simulate physics and respond to entity contacts.
-}
simulate :
    Float
    -> List Wall
    -> Memory
    -> Memory
simulate dt walls memory =
    let
        ( entities, contacts ) =
            Physics.step config dt walls memory.entities
    in
    List.foldl
        (\contact memoryAccum ->
            case contact of
                BetweenBodies id1 id2 data ->
                    Entity.respond id1 id2 data memoryAccum

                WithWall _ _ ->
                    -- Ignore wall contacts
                    memoryAccum
        )
        { memory | entities = entities }
        contacts


updateCamera : Float -> Memory -> Memory
updateCamera dt memory =
    let
        maybePlayer =
            Entity.player memory.entities
    in
    case maybePlayer of
        Just player ->
            { memory
                | camera = Camera.follow config dt player.p memory.camera
            }

        Nothing ->
            memory


{-| Attempt to init camera with player spawn position.
-}
findCameraPosition : List Spawn -> Camera
findCameraPosition spawns =
    let
        maybeSpawn =
            List.filter (\spawn -> spawn.type_ == Player) spawns
                |> List.head
    in
    case maybeSpawn of
        Just spawn ->
            Camera.init spawn.p

        Nothing ->
            Camera.init Vec2.zero



-- ENTRY POINT


main =
    game view update (init Levels.level1)
