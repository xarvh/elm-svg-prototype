module Editor exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Svg exposing (Svg, g)
import Svg.Attributes.Typed exposing (..)
import Svg.Events


rotateVector : Float -> Vec2 -> Vec2
rotateVector angle v =
    let
        ( x, y ) =
            Vec2.toTuple v

        sinA =
            sin -angle

        cosA =
            cos angle
    in
    vec2
        (x * cosA - y * sinA)
        (x * sinA + y * cosA)



-- Super-Generic Nodes


type Node transform payload
    = Node payload (List ( transform, Node transform payload ))


expandNode : (t -> t -> t) -> t -> ( t, Node t payload ) -> List ( t, payload )
expandNode chainTransform parentWorldTransform ( transform, Node payload children ) =
    let
        worldTransform =
            chainTransform parentWorldTransform transform

        expandedNode =
            ( worldTransform, payload )
    in
    expandedNode :: List.concat (List.map (expandNode chainTransform worldTransform) children)



-- Herzog Drei-specific stuff


type alias Transform =
    { translation : Vec2
    , rotation : Float
    }


chainTransform : Transform -> Transform -> Transform
chainTransform parent child =
    { translation = child.translation |> rotateVector parent.rotation |> Vec2.add parent.translation
    , rotation = parent.rotation + child.rotation
    }



-- Mantis-specific stuff


type Side
    = Left
    | Right


type MantisBody
    = Torso
    | Shoulder Side
    | Elbow Side
    | Wrist Side


type alias MantisFrame =
    { torsoA : Float
    , leftShoulderA : Float
    , leftElbowA : Float
    }


type alias MantisNode =
    Node Transform MantisBody



--


halfTorsoWidth =
    0.2


upperArmLength =
    0.2

lowerArmLength =
    0.3

open1 : MantisFrame
open1 =
    { torsoA = 0
    , leftShoulderA = pi
    , leftElbowA = 0
    }



--


mantisRig : MantisFrame -> MantisNode
mantisRig frame =
    Node Torso
        [ ( { translation = vec2 -halfTorsoWidth 0
            , rotation = -pi + pi / 6
            }
          , Node (Shoulder Left)
                [ ( { translation = vec2 upperArmLength 0
                    , rotation = pi / 4
                    }
                  , Node (Elbow Left)
                        []
                  )
                ]
          )
        ]


rect tr id cx cy w h color =
    Svg.rect
        [ x <| cx - w / 2
        , y <| cy - h / 2
        , width w
        , height h
        , Svg.Events.onClick (Select id)
        , tr
        , fill color
        , opacity 0.8
        ]
        []


renderMantisNode : Vec2 -> Float -> MantisBody -> Svg Msg
renderMantisNode position angle bodyId =
    let
        tr =
            transform [ translate position, rotateRad angle ]
    in
    case bodyId of
        Torso ->
            rect tr bodyId 0 0 (halfTorsoWidth * 2) 0.2 "red"

        Shoulder side ->
            rect tr bodyId (upperArmLength / 2) 0 upperArmLength 0.1 "blue"

        Elbow side ->
            rect tr bodyId (lowerArmLength / 2) 0 lowerArmLength 0.06 "green"

        _ ->
            Debug.crash "WTF"



-- TEA


type Msg
    = Noop
    | Select MantisBody


type alias Model =
    {}



-- init


init : Model
init =
    {}



-- update


update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model

        Select body ->
            let
                q =
                    Debug.log "" body
            in
            model



-- view


view : Model -> Svg Msg
view model =
    let
        unitTransform =
            { translation = vec2 0 0
            , rotation = 0
            }

        torsoTransform =
            { translation = vec2 0 0
            , rotation = 0
            }

        nodes =
            expandNode chainTransform unitTransform ( torsoTransform, mantisRig open1 )
    in
    nodes
        |> List.map (\( { translation, rotation }, id ) -> renderMantisNode translation rotation id)
        |> g [ transform [ scale 0.5 ] ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
