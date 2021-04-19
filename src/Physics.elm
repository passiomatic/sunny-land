module Physics exposing
    ( Contact(..)
    , ContactData
    , PhysicsBody
    , Wall
    , addImpulse
    , step
    )

{-| Game physics.

Most of the code is adapted from "Game Physics Engine Development" by Ian Millington.
<https://www.amazon.it/Game-Physics-Engine-Development-Commercial-Grade/dp/0123819768>

Original C++ sources are available on Github at:
<https://github.com/idmillington/cyclone-physics>

About stability "Slops":
<http://allenchou.net/2014/01/game-physics-stability-slops/>

-}

import AltMath.Vector2 as Vec2 exposing (Vec2, vec2)
import Bitwise as B
import Dict exposing (Dict)
import Vector2.Extra as Vec2


{-| Walls and ground in the game. They have a direction: if p2 > p1 the contact normal points "up".
-}
type alias Wall =
    { p1 : Vec2, p2 : Vec2, normal : Vec2 }


{-| A "physics body" is the set of attributes of a game entity which contributes to physics simulation.
-}
type alias PhysicsBody a =
    { a
        | id : Int
        , p : Vec2
        , v : Vec2
        , a : Vec2
        , cumulativeImpulse : Vec2
        , cumulativeContact : Vec2
        , radius : Float
        , restitution : Float
        , contactTestBitMask : Int
        , categoryBitMask : Int
        , affectedByGravity : Bool
        , affectedByContact : Bool
    }


type alias ContactData =
    { normal : Vec2
    , penetration : Float
    }


type Contact a
    = BetweenBodies Int Int ContactData
    | WithWall Int ContactData


step :
    { b | friction : Vec2, g : Vec2 }
    -> Float
    -> List Wall
    -> Dict Int (PhysicsBody a)
    -> ( Dict Int (PhysicsBody a), List (Contact a) )
step config dt walls bodies =
    let
        newEntries =
            Dict.map (\_ body -> integrate config dt body) bodies

        contacts =
            Dict.foldl
                (\_ body accum ->
                    accum
                        |> computeContactWithWalls body walls
                        |> computeContactWithEntities body newEntries
                )
                []
                newEntries
    in
    ( resolveContacts contacts newEntries, contacts )


integrate : { b | friction : Vec2, g : Vec2 } -> Float -> PhysicsBody a -> PhysicsBody a
integrate config dt body =
    let
        -- TODO integrate with dt
        a =
            body.a
                |> Vec2.add body.cumulativeImpulse
                |> (if body.affectedByGravity then
                        Vec2.add config.g

                    else
                        Vec2.add Vec2.zero
                   )
    in
    { body
        | p = Vec2.add body.p body.v
        , v =
            Vec2.add body.v a
                -- Stokes' drag https://stackoverflow.com/a/667090
                |> Vec2.add (Vec2.mul config.friction body.v)
        , cumulativeImpulse = Vec2.zero
        , cumulativeContact = Vec2.zero
    }



-- iterate: List (Contact a) -> Dict Int (PhysicsBody a) -> Dict Int (PhysicsBody a)
-- iterate contacts entities =


resolveContacts :
    List (Contact a)
    -> Dict Int (PhysicsBody a)
    -> Dict Int (PhysicsBody a)
resolveContacts contacts bodies =
    List.foldl
        (\contact accum ->
            case contact of
                WithWall id contact_ ->
                    case Dict.get id accum of
                        Just body ->
                            let
                                newBody =
                                    resolveBodyVsWallContact contact_ body
                            in
                            accum
                                |> Dict.insert id newBody

                        _ ->
                            accum

                BetweenBodies id1 id2 contact_ ->
                    case ( Dict.get id1 accum, Dict.get id2 accum ) of
                        ( Just body1, Just body2 ) ->
                            let
                                ( newBody1, newBody2 ) =
                                    if not body1.affectedByContact || not body2.affectedByContact then
                                        -- Do not resolve contact, let entity logic to handle it
                                        ( body1, body2 )

                                    else
                                        resolveBodyVsBodyContact contact_ body1 body2
                            in
                            accum
                                |> Dict.insert newBody1.id newBody1
                                |> Dict.insert newBody2.id newBody2

                        ( _, _ ) ->
                            accum
        )
        bodies
        contacts


computeContactWithWalls :
    PhysicsBody a
    -> List Wall
    -> List (Contact a)
    -> List (Contact a)
computeContactWithWalls body walls accum =
    List.foldl
        (\wall accumContact ->
            case contactWithWall body wall of
                Just contact ->
                    contact :: accumContact

                Nothing ->
                    accumContact
        )
        accum
        walls


computeContactWithEntities :
    PhysicsBody a
    -> Dict Int (PhysicsBody a)
    -> List (Contact a)
    -> List (Contact a)
computeContactWithEntities body others accum =
    Dict.foldl
        (\id other accumContact ->
            -- Make sure to:
            -- * Skip certain entity-vs-entity combinations according to contactTestBitMask
            -- * Don't check with itself
            -- * Do not generate (M,N) (N,M) contact dupes
            if B.and body.categoryBitMask other.contactTestBitMask /= 0 && B.and other.categoryBitMask body.contactTestBitMask /= 0 && body.id < id then
                case contactWithBody body other of
                    Just contact ->
                        contact :: accumContact

                    Nothing ->
                        accumContact

            else
                accumContact
        )
        accum
        others



-- COLLISION RESOLUTION


slop =
    0.005


{-| Resolve contact between two physics bodies (circles).
-}
resolveBodyVsBodyContact : ContactData -> PhysicsBody a -> PhysicsBody a -> ( PhysicsBody a, PhysicsBody a )
resolveBodyVsBodyContact { normal, penetration } body1 body2 =
    let
        -- Bodies relative velocity along normal
        vel =
            Vec2.dot (Vec2.sub body2.v body1.v) normal
    in
    if vel > 0 then
        -- Do not resolve if bodies are separating
        ( body1, body2 )

    else
        let
            -- Halve velocity to distribute it equally between both bodies
            impulse =
                Vec2.scale (-1 * vel / 2) normal
        in
        ( { body1
            | v = Vec2.sub body1.v impulse
            , p = Vec2.sub body1.p (Vec2.scale (max (penetration / 2 - slop) 0.0) normal)
            , cumulativeContact = Vec2.add body1.cumulativeContact impulse
          }
        , { body2
            | v = Vec2.add body2.v impulse
            , p = Vec2.add body2.p (Vec2.scale (max (penetration / 2 - slop) 0.0) normal)
            , cumulativeContact = Vec2.add body2.cumulativeContact impulse
          }
        )


{-| Resolve contact between physics body (circle) and wall (segment).
-}
resolveBodyVsWallContact : ContactData -> PhysicsBody a -> PhysicsBody a
resolveBodyVsWallContact { normal, penetration } body1 =
    let
        -- Wall has zero velocity, ignore
        vel =
            Vec2.dot body1.v normal
    in
    if vel > 0 then
        -- Do not resolve if body is separating from wall
        body1

    else
        let
            impulse =
                Vec2.scale (-1 * vel) normal
        in
        { body1
            | v = Vec2.add body1.v impulse
            , p = Vec2.add body1.p (Vec2.scale (max (penetration - slop) 0.0) normal)
            , cumulativeContact = Vec2.add body1.cumulativeContact impulse
        }



-- COLLISION DETECTION


{-| Calculate intersection between physics body (circle) and wall (segment).
See: <https://stackoverflow.com/a/1079478>
-}
contactWithWall : PhysicsBody a -> Wall -> Maybe (Contact a)
contactWithWall body wall =
    -- Check if body is coming from behind/below the wall
    if Vec2.dot body.v wall.normal > 0 then
        Nothing

    else
        let
            -- Vector to start of wall to body center
            dir =
                Vec2.sub body.p wall.p1

            -- Distance from body position along normal
            distanceSq =
                Vec2.projection dir wall.normal
                    |> Vec2.lengthSquared
        in
        if distanceSq <= body.radius ^ 2 then
            let
                w =
                    Vec2.sub wall.p2 wall.p1

                edgeDistance =
                    Vec2.dot dir (Vec2.normalize w)
            in
            -- On p1 edge or past p2 edge?
            if edgeDistance < 0 || edgeDistance > Vec2.length w then
                -- Let fall
                Nothing

            else
                -- Not on edges, body is touching wall
                Just
                    (WithWall
                        body.id
                        { normal = wall.normal
                        , penetration = body.radius - sqrt distanceSq
                        }
                    )

        else
            -- Body not touching wall
            Nothing


{-| Calculate contact between two physics bodies (circles).
-}
contactWithBody : PhysicsBody a -> PhysicsBody a -> Maybe (Contact a)
contactWithBody body1 body2 =
    let
        v =
            Vec2.sub body2.p body1.p
    in
    if Vec2.lengthSquared v > (body1.radius + body2.radius) ^ 2 then
        Nothing

    else
        let
            d =
                Vec2.length v
        in
        if d /= 0 then
            -- Touching
            Just
                (BetweenBodies
                    body1.id
                    body2.id
                    { normal = Vec2.normalize v
                    , penetration = (body1.radius + body2.radius) - d
                    }
                )

        else
            -- Bodies perfectly overlap, arbitrary pick one
            Just
                (BetweenBodies
                    body1.id
                    body2.id
                    { normal = vec2 1 0
                    , penetration = body1.radius
                    }
                )


{-| Apply accelaration to a physics body.
-}
addImpulse :
    Vec2
    -> PhysicsBody a
    -> PhysicsBody a
addImpulse value body =
    { body
        | cumulativeImpulse = Vec2.add body.cumulativeImpulse value
    }
