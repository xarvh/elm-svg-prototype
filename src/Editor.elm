module Editor exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Svg exposing (Svg, g)
import Svg.Attributes.Typed exposing (..)


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



-- Nodes


type Node id frame
    = Node
        { id : id
        , origin : frame -> Vec2
        , rotation : frame -> Float
        }
        (List (Node id frame))


drawRig : Vec2 -> Float -> (Vec2 -> Float -> id -> renderArtefact) -> frame -> Node id frame -> List renderArtefact
drawRig parentAbsoluteOrigin parentAbsoluteRotation renderNode frame (Node node children) =
    let
        absoluteRotation =
            parentAbsoluteRotation + node.rotation frame

        offset =
            node.origin frame |> rotateVector absoluteRotation

        absoluteOrigin =
            Vec2.add parentAbsoluteOrigin offset

        drawChildren =
            drawRig absoluteOrigin absoluteRotation renderNode frame
    in
    renderNode absoluteOrigin absoluteRotation node.id :: List.concat (List.map drawChildren children)



-- mantis rig


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


halfTorsoWidth =
    0.4


upperArmLength =
    0.2


leftShoulderOrigin =
    \_ -> vec2 -halfTorsoWidth 0


leftElbowOrigin =
    \_ -> vec2 -upperArmLength 0


mantisRig : Node MantisBody MantisFrame
mantisRig =
    Node
        { id = Torso
        , origin = always (vec2 0 0)
        , rotation = .torsoA
        }
        [ Node
            { id = Shoulder Left
            , origin = leftShoulderOrigin
            , rotation = .leftShoulderA
            }
            [ Node
                { id = Elbow Left
                , origin = leftElbowOrigin
                , rotation = .leftElbowA
                }
                []
            ]
        ]



-- TEA


type Msg
    = Noop


type alias Model =
    {}



-- init


init : Model
init =
    {}



-- update


update : Msg -> Model -> Model
update msg model =
    model



-- view


view : Model -> Svg Msg
view model =
    Svg.g [] []



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
