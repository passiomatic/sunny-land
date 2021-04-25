module Entity exposing
    ( Entity
    , EntityType(..)
    , Status(..)
    , fromSpawns
    , player
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


type EntityType
    = Eagle
    | Opossum
    | Player
    | Gem
    | Cherry


type Status
    = Normal
    | Hit Int
    | Removing Int


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
    , status : Status
    , health : Float
    , attackDamage : Float
    , points : Int
    }


defaultEntity =
    { id = 1 -- Reserve 0 for player
    , p = Vec2.zero
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
    , dir = Neither
    , type_ = Player
    , status = Normal
    , health = 0
    , attackDamage = 0
    , points = 0
    }


fromSpawns : List { p : Vec2, dir : Direction, type_ : EntityType } -> Dict Int Entity
fromSpawns spawns =
    List.indexedMap
        (\id spawn ->
            let
                nextId =
                    id + 1
            in
            case spawn.type_ of
                Player ->
                    -- Hardcode id=0 for player
                    ( 0, spawnPlayer spawn.p spawn.dir 0 )

                Eagle ->
                    ( nextId, spawnEagle spawn.p spawn.dir nextId )

                Opossum ->
                    ( nextId, spawnOpossum spawn.p spawn.dir nextId )

                Gem ->
                    ( nextId, spawnItemAt spawn nextId )

                Cherry ->
                    ( nextId, spawnCherry spawn nextId )
        )
        spawns
        |> Dict.fromList


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


player : Dict Int Entity -> Maybe Entity
player entities =
    Dict.get 0 entities


spawnPlayer : Vec2 -> Direction -> Int -> Entity
spawnPlayer position dir id =
    { defaultEntity
        | id = id
        , p = position
        , contactTestBitMask = enemyCategory + itemCategory
        , categoryBitMask = playerCategory
        , radius = 13
        , dir = dir
        , health = 1.0
        , attackDamage = 0.5
        , type_ = Player
    }


renderPlayer : Time -> Entity -> Shape
renderPlayer time entity =
    let
        sprite =
            case entity.status of
                Removing _ ->
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
    --Playground.move (pxSnap entity.p.x) (pxSnap entity.p.y) sprite
    Playground.move entity.p.x entity.p.y sprite



-- ENEMIES


enemyCategory =
    2


spawnOpossum : Vec2 -> Direction -> Int -> Entity
spawnOpossum position dir id =
    { defaultEntity
        | id = id
        , p = position
        , contactTestBitMask = playerCategory
        , categoryBitMask = enemyCategory
        , radius = 15
        , dir = dir
        , health = 0.4
        , attackDamage = 0.2
        , points = 200
        , type_ = Opossum
    }


renderOpossum : Time -> Entity -> Shape
renderOpossum time entity =
    let
        sprite =
            case entity.status of
                Removing _ ->
                    Sprites.enemyDie entity.dir time

                _ ->
                    Sprites.opossumWalk entity.dir time
    in
    Playground.move entity.p.x entity.p.y sprite


spawnEagle : Vec2 -> Direction -> Int -> Entity
spawnEagle position dir id =
    { defaultEntity
        | id = id
        , p = position
        , affectedByGravity = False
        , contactTestBitMask = playerCategory
        , categoryBitMask = enemyCategory
        , radius = 15
        , dir = dir
        , health = 0.7
        , attackDamage = 0.4
        , points = 500
        , type_ = Eagle
    }


renderEagle : Time -> Entity -> Shape
renderEagle time entity =
    let
        sprite =
            case entity.status of
                Removing _ ->
                    Sprites.enemyDie entity.dir time

                _ ->
                    Sprites.eagleIdle entity.dir time
    in
    Playground.move entity.p.x entity.p.y sprite



-- ITEMS


itemCategory =
    4


spawnItemAt : { a | p : Vec2, dir : Direction } -> Int -> Entity
spawnItemAt spawn id =
    { defaultEntity
        | id = id
        , p = spawn.p
        , contactTestBitMask = playerCategory
        , categoryBitMask = itemCategory
        , radius = 7
        , type_ = Gem
        , status = Normal
        , points = 1000
        , affectedByGravity = False
        , affectedByContact = False
    }


renderGem =
    renderItem Sprites.gem


spawnCherry : { a | p : Vec2, dir : Direction } -> Int -> Entity
spawnCherry spawn id =
    { defaultEntity
        | id = id
        , p = spawn.p
        , contactTestBitMask = playerCategory
        , categoryBitMask = itemCategory
        , radius = 9
        , type_ = Cherry
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
    -> { a | entities : Dict Int Entity, collectedGems : Int, fx : Fx }
    -> { a | entities : Dict Int Entity, collectedGems : Int, fx : Fx }
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
                        |> pickUpItem entity1 entity2

                -- Pick up cherry
                ( Player, Cherry ) ->
                    pickUpItem entity1 entity2 memory

                ( _, _ ) ->
                    memory

        ( _, _ ) ->
            memory


hasStomped contact =
    contact.normal.y < -0.7 && contact.normal.y > -1.3


pickUpItem player_ item memory =
    let
        newPlayer =
            { player_
                | points = player_.points + item.points
            }
    in
    memory
        |> setEntity newPlayer
        |> setEntity (remove item)


{-| Figure out if player or enemy is attacking and apply damage accordingly.
-}
resolveAttack player_ enemy contact memory =
    if hasStomped contact then
        let
            newHealth =
                enemy.health - player_.attackDamage

            ( newPlayer, newEnemy ) =
                if newHealth <= 0 then
                    ( { player_
                        | points = player_.points + enemy.points
                      }
                    , { enemy
                        | status = Removing 600
                        , contactTestBitMask = 0
                        , a = Vec2.zero
                      }
                    )

                else
                    ( player_
                    , { enemy
                        | health = newHealth
                        , status = Hit 400
                      }
                    )
        in
        memory
            |> setEntity (Physics.addImpulse (vec2 0 5.5) newPlayer)
            |> setEntity newEnemy

    else
        -- Hit by enemy
        memory
            |> setEntity (damagePlayer enemy.attackDamage enemy.p player_)
            |> setFx Fx.flash


damagePlayer : Float -> Vec2 -> Entity -> Entity
damagePlayer value target entity =
    let
        -- Push player away from attacker
        side =
            if (Vec2.sub target entity.p |> Vec2.getX) > 0 then
                -1

            else
                1
    in
    { entity
        | health = entity.health - value
        , status = Hit 400
        , a = Vec2.zero
    }
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
                        ( Player, Hit timeout ) ->
                            let
                                remaining =
                                    timeout - time.delta

                                status =
                                    if remaining > 0 then
                                        Hit remaining

                                    else
                                        Normal
                            in
                            Dict.insert id
                                { entity
                                    | status = status
                                }
                                accum

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

                                -- else
                                --     ( vec2 ax entity.a.y, entity.v )
                            in
                            Dict.insert id
                                { entity
                                    | a = a
                                    , v = v
                                    , dir = dir
                                }
                                accum

                        -- ########
                        -- All entities
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

                            else
                                -- Done, remove from world
                                accum

                        ( _, _ ) ->
                            -- Leave entity as-is
                            Dict.insert id entity accum
                )
                Dict.empty
                memory.entities
    }



-- HELPERS


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
