module Entity exposing
    ( Entity
    , EntityStatus(..)
    , EntityType(..)
    , Spawn
    , fromSpawns
    , getPlayer
    , isReady
    , render
    , respond
    , update
    )

{-| Game entities and related logic.
-}

import AltMath.Vector2 as Vec2 exposing (Vec2, vec2)
import Dict exposing (Dict)
import Direction exposing (Direction(..))
import Fx exposing (Fx(..))
import Physics exposing (ContactData)
import Playground exposing (Computer, Keyboard, Shape, Time)
import Sprites
import Vector2.Extra as Vec2


type alias Entity =
    { id : Int
    , p : Vec2
    , v : Vec2
    , a : Vec2
    , cumulativeImpulse : Vec2
    , cumulativeContact : Vec2
    , radius : Float
    , restitution : Float
    , affectedByContact : Bool
    , affectedByGravity : Bool
    , contactTestBitMask : Int
    , categoryBitMask : Int
    , dir : Direction
    , type_ : EntityType
    , status : EntityStatus
    , health : Float
    , attackDamage : Float
    , points : Int
    , spawn : Spawn
    }


type EntityType
    = Eagle
    | Opossum
    | Player
    | Gem
    | Cherry


type EntityStatus
    = Normal
    | Hit Int
    | Removing Int
    | Waiting Int
    | Spawning Int


{-| Spawn point for a game entity.
-}
type alias Spawn =
    { p : Vec2
    , dir : Direction
    , type_ : EntityType
    }


fromSpawns : List Spawn -> Dict Int Entity
fromSpawns spawns =
    List.indexedMap
        (\index spawn_ ->
            spawn spawn_ (index + 1)
        )
        spawns
        |> Dict.fromList


spawn =
    spawnHelp Normal


spawnAfter timeout =
    spawnHelp (Waiting timeout)


spawnHelp : EntityStatus -> Spawn -> Int -> ( Int, Entity )
spawnHelp status spawn_ nextId =
    let
        entity =
            { id = nextId
            , p = spawn_.p
            , v = Vec2.zero
            , a = Vec2.zero
            , cumulativeImpulse = Vec2.zero
            , cumulativeContact = Vec2.zero
            , radius = 10
            , restitution = 1.0
            , affectedByGravity = True
            , affectedByContact = True
            , contactTestBitMask = 0
            , categoryBitMask = 0
            , dir = spawn_.dir
            , type_ = spawn_.type_
            , status = status
            , health = 0
            , attackDamage = 0
            , points = 0
            , spawn = spawn_
            }
    in
    case spawn_.type_ of
        Player ->
            ( 0, initPlayer entity )

        Eagle ->
            ( nextId, initEagle entity )

        Opossum ->
            ( nextId, initOpossum entity )

        Gem ->
            ( nextId, initGem entity )

        Cherry ->
            ( nextId, initCherry entity )


render : Time -> Dict Int Entity -> List Shape
render time entities =
    Dict.map
        (\_ entity ->
            case entity.type_ of
                Player ->
                    renderPlayer time entity

                Eagle ->
                    renderEagle time entity

                Opossum ->
                    renderOpossum time entity

                Gem ->
                    renderGem time entity

                Cherry ->
                    renderCherry time entity
        )
        entities
        |> Dict.values



-- PLAYER


{-| Player acceleration on ground.
-}
playerAcceleration =
    0.29


{-| Player jump vertical thrust.
-}
playerJump =
    6.3


playerCategory =
    1


getPlayer : Dict Int Entity -> Maybe Entity
getPlayer entities =
    Dict.get 0 entities


initPlayer : Entity -> Entity
initPlayer entity =
    { entity
        | id = 0 -- Hardcode value for player
        , contactTestBitMask = enemyCategory + itemCategory
        , categoryBitMask = playerCategory
        , radius = 13
        , health = 1.0
        , attackDamage = 0.5
    }


renderPlayer : Time -> Entity -> Shape
renderPlayer time entity =
    let
        sprite =
            case entity.status of
                Removing _ ->
                    -- Reuse hit sprites
                    Sprites.heroHit entity.dir time

                Hit _ ->
                    Sprites.heroHit entity.dir time

                _ ->
                    if entity.v.y > 0.2 && entity.cumulativeContact.y == 0 then
                        -- Not touching the ground, jumping
                        Sprites.heroJump entity.dir

                    else if entity.v.y < -0.2 && entity.cumulativeContact.y == 0 then
                        -- Not touching the ground, falling
                        Sprites.heroFall entity.dir

                    else if entity.v.x > 0.2 || entity.v.x < -0.2 then
                        -- Running
                        Sprites.heroRun entity.dir time

                    else
                        -- Idle
                        Sprites.heroIdle entity.dir time
    in
    Playground.move entity.p.x entity.p.y sprite



-- ENEMIES


enemyCategory =
    2


initOpossum : Entity -> Entity
initOpossum entity =
    { entity
        | contactTestBitMask = playerCategory
        , categoryBitMask = enemyCategory
        , radius = 15
        , health = 0.4
        , attackDamage = 0.2
        , points = 200
    }


renderOpossum : Time -> Entity -> Shape
renderOpossum time entity =
    let
        sprite =
            case entity.status of
                Waiting _ ->
                    none

                Removing _ ->
                    Sprites.enemyDie entity.dir time

                Spawning _ ->
                    Sprites.enemySpawn entity.dir time

                _ ->
                    Sprites.opossumWalk entity.dir time
    in
    Playground.move entity.p.x entity.p.y sprite


initEagle : Entity -> Entity
initEagle entity =
    { entity
        | affectedByGravity = False
        , contactTestBitMask = playerCategory
        , categoryBitMask = enemyCategory
        , radius = 15
        , health = 0.7
        , attackDamage = 0.4
        , points = 500
    }


renderEagle : Time -> Entity -> Shape
renderEagle time entity =
    let
        sprite =
            case entity.status of
                Removing _ ->
                    Sprites.enemyDie entity.dir time

                Spawning _ ->
                    Sprites.enemySpawn entity.dir time

                _ ->
                    Sprites.eagleIdle entity.dir time
    in
    Playground.move entity.p.x entity.p.y sprite



-- ITEMS


itemCategory =
    4


initGem : Entity -> Entity
initGem entity =
    { entity
        | contactTestBitMask = playerCategory
        , categoryBitMask = itemCategory
        , radius = 7
        , points = 1000
        , affectedByGravity = False
        , affectedByContact = False
    }


renderGem =
    renderItem Sprites.gem


initCherry : Entity -> Entity
initCherry entity =
    { entity
        | contactTestBitMask = playerCategory
        , categoryBitMask = itemCategory
        , radius = 9
        , points = 150
        , affectedByGravity = False
        , affectedByContact = False
    }


renderCherry =
    renderItem Sprites.cherry


renderItem : (Direction -> Time -> Shape) -> Time -> Entity -> Shape
renderItem fn time entity =
    let
        sprite_ =
            case entity.status of
                Removing _ ->
                    Sprites.itemFeedback entity.dir time

                _ ->
                    fn entity.dir time
    in
    Playground.move entity.p.x entity.p.y sprite_



-- CONTACT


{-| Contact response logic.
-}
respond :
    Int
    -> Int
    -> ContactData
    -> { a | entities : Dict Int Entity, score : Int, collectedGems : Int, fx : Fx }
    -> { a | entities : Dict Int Entity, score : Int, collectedGems : Int, fx : Fx }
respond id1 id2 contact memory =
    case ( Dict.get id1 memory.entities, Dict.get id2 memory.entities ) of
        ( Just entity1, Just entity2 ) ->
            case ( entity1.type_, entity2.type_ ) of
                ( Player, Opossum ) ->
                    case entity1.status of
                        Hit _ ->
                            -- Don't hit again
                            memory

                        _ ->
                            resolveAttack entity1 entity2 contact memory

                -- ( Player, Eagle ) ->
                --     case entity1.status of
                --         Hit _ ->
                --             -- Don't hit again
                --             memory
                --         _ ->
                --             memory
                --                 |> setEntity (damagePlayer entity2.attackDamage entity2.p entity1)
                --                 |> setFx Fx.flash
                -- Pick up gem
                ( Player, Gem ) ->
                    { memory
                        | collectedGems = memory.collectedGems + 1
                    }
                        |> pickUpItem entity2

                -- Pick up cherry
                ( Player, Cherry ) ->
                    memory
                        |> pickUpItem entity2

                ( _, _ ) ->
                    memory

        ( _, _ ) ->
            memory


hasStomped contact =
    contact.normal.y < -0.7 && contact.normal.y > -1.3


pickUpItem item memory =
    { memory
        | score = memory.score + item.points
    }
        |> setEntity (remove item)


{-| Figure out if player or enemy is attacking and apply damage accordingly.
-}
resolveAttack player enemy contact memory =
    if hasStomped contact then
        let
            newHealth =
                enemy.health - player.attackDamage

            newPlayer =
                Physics.addImpulse (vec2 0 5.5) player

            newEnemy =
                if newHealth > 0.1 then
                    { enemy
                        | health = newHealth
                        , status = Hit 400
                    }

                else
                    { enemy
                        | status = Removing 600
                        , contactTestBitMask = 0
                        , a = Vec2.zero
                    }
        in
        { memory
            | score = memory.score + enemy.points
        }
            |> setEntity newPlayer
            |> setEntity newEnemy

    else
        -- Hit by enemy
        memory
            |> setEntity (damagePlayer enemy.attackDamage enemy.p player)
            |> setFx Fx.flash


damagePlayer : Float -> Vec2 -> Entity -> Entity
damagePlayer value target player =
    let
        newHealth =
            player.health - value

        -- Target position relative to player
        side =
            if (Vec2.sub target player.p |> Vec2.getX) > 0 then
                -1

            else
                1

        newPlayer =
            if newHealth > 0.01 then
                { player
                    | health = newHealth
                    , status = Hit 400
                    , a = Vec2.zero
                }

            else
                -- Player is dead
                { player
                    | health = newHealth
                    , status = Removing 3500
                    , a = Vec2.zero
                    , affectedByContact = False
                }
    in
    -- Push player away from attacker
    newPlayer
        |> Physics.addImpulse (vec2 (side * 3.5) 4.5)


remove : Entity -> Entity
remove entity =
    { entity
        | status = Removing 340
        , contactTestBitMask = 0
    }



-- UPDATE


{-| Figure out next status for each entity.
-}
update : Computer -> { b | g : Vec2 } -> { a | entities : Dict Int Entity } -> { a | entities : Dict Int Entity }
update { keyboard, time } config memory =
    { memory
        | entities =
            Dict.foldl
                (\id entity accum ->
                    case ( entity.type_, entity.status ) of
                        -- ########
                        -- Eagle
                        -- ########
                        -- ( Eagle, Normal ) ->
                        --     let
                        --         py =
                        --             entity.p.y + Playground.zigzag -1.5 1.5 2 time
                        --     in
                        --     Dict.insert id
                        --         { entity
                        --             | p = vec2 entity.p.x py
                        --         }
                        --         accum
                        -- ########
                        -- Opossum
                        -- ########
                        ( Opossum, Normal ) ->
                            let
                                newDir =
                                    -- Keep walking along direction until wall/enemy is hit
                                    if entity.cumulativeContact.x /= 0 then
                                        Direction.opposite entity.dir

                                    else
                                        entity.dir

                                ax =
                                    case newDir of
                                        East ->
                                            0.11

                                        West ->
                                            -0.11

                                        _ ->
                                            0
                            in
                            Dict.insert id
                                { entity
                                    | dir = newDir
                                    , a = vec2 ax 0
                                }
                                accum

                        -- ########
                        -- Player
                        -- ########
                        ( Player, Normal ) ->
                            let
                                dir =
                                    direction keyboard entity.dir

                                ax =
                                    if keyboard.left then
                                        -playerAcceleration

                                    else if keyboard.right then
                                        playerAcceleration

                                    else
                                        0

                                ( a, v ) =
                                    if keyboard.space && entity.cumulativeContact.y >= abs config.g.y then
                                        -- On the ground, can jump
                                        ( vec2 ax playerJump, Vec2.setY 0 entity.v )

                                    else
                                        -- On air, don't jump again
                                        ( vec2 ax 0, entity.v )
                            in
                            Dict.insert id
                                { entity
                                    | a = a
                                    , v = v
                                    , dir = dir
                                }
                                accum

                        ( Player, Hit timeout ) ->
                            let
                                remaining =
                                    timeout - time.delta
                            in
                            changeStatus remaining (Hit remaining) Normal entity accum

                        ( Player, Removing timeout ) ->
                            let
                                remaining =
                                    timeout - time.delta
                            in
                            -- Keep player in removing state as we check for end game
                            Dict.insert id { entity | status = Removing remaining } accum

                        -- ########
                        -- Other entities
                        -- ########
                        ( _, Removing timeout ) ->
                            let
                                remaining =
                                    timeout - time.delta
                            in
                            if remaining > 0 then
                                Dict.insert id
                                    { entity
                                        | status = Removing remaining
                                    }
                                    accum

                            else if entity.type_ == Opossum then
                                let
                                    -- Respawn after timeout
                                    ( _, newEntity ) =
                                        spawnAfter 8000 entity.spawn entity.id
                                in
                                Dict.insert id newEntity accum

                            else
                                -- Done, remove from world
                                accum

                        ( _, Waiting timeout ) ->
                            let
                                remaining =
                                    timeout - time.delta
                            in
                            changeStatus remaining (Waiting remaining) (Spawning 600) entity accum

                        ( _, Spawning timeout ) ->
                            let
                                remaining =
                                    timeout - time.delta
                            in
                            changeStatus remaining (Spawning remaining) Normal entity accum

                        ( _, _ ) ->
                            -- Leave entity as-is
                            Dict.insert id entity accum
                )
                Dict.empty
                memory.entities
    }


changeStatus remaining current next entity accum =
    let
        newStatus =
            if remaining > 0 then
                current

            else
                next
    in
    Dict.insert entity.id
        { entity
            | status = newStatus
        }
        accum



-- HELPERS


none : Shape
none =
    Playground.group []


{-| Is entity ready for simulation?
-}
isReady : Entity -> Bool
isReady entity =
    case entity.status of
        Waiting _ ->
            False

        Spawning _ ->
            False

        _ ->
            True


{-| Figure out entity direction from keyboard status
-}
direction : Keyboard -> Direction -> Direction
direction keyboard dir =
    let
        newDir =
            Direction.fromTuple (Playground.toXY keyboard)
    in
    if newDir /= Direction.Neither then
        newDir

    else
        -- Keep last direction given while idle
        dir


setEntity entity memory =
    { memory
        | entities =
            memory.entities
                |> Dict.insert entity.id entity
    }


setFx fx memory =
    -- Don't cut ongoing fx
    if fx /= None then
        { memory
            | fx = fx
        }

    else
        memory


setStatus status memory =
    { memory
        | status = status
    }
