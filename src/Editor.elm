module Editor exposing (..)

import Array exposing (Array)
import Html
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Mouse
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
    = Node payload transform (List (Node transform payload))


expandNode : (t -> t -> t) -> t -> Node t payload -> List ( t, payload )
expandNode chainTransform parentWorldTransform (Node payload transform children) =
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
    , leftWristA : Float
    }


type alias MantisNode =
    Node Transform MantisBody


open1 : MantisFrame
open1 =
    { torsoA = 0
    , leftShoulderA = pi
    , leftElbowA = 0
    , leftWristA = 0
    }


updateSelectedFrame : ( Float, Float ) -> MantisBody -> MantisFrame -> MantisFrame
updateSelectedFrame ( dx, dy ) nodeId frame =
    case nodeId of
        Torso ->
            { frame | torsoA = frame.torsoA + 0.005 * dx }

        Shoulder Left ->
            { frame | leftShoulderA = frame.leftShoulderA + 0.005 * dx }

        Elbow Left ->
            { frame | leftElbowA = frame.leftElbowA + 0.005 * dx }

        Wrist Left ->
            { frame | leftWristA = frame.leftWristA + 0.005 * dx }

        _ ->
            frame



--


halfTorsoWidth =
    0.2


upperArmLength =
    0.2


lowerArmLength =
    0.3


mantisRig : MantisFrame -> MantisNode
mantisRig frame =
    Node Torso
        { translation = vec2 0 0
        , rotation = frame.torsoA
        }
        [ Node (Shoulder Left)
            { translation = vec2 -halfTorsoWidth 0
            , rotation = frame.leftShoulderA
            }
            [ Node (Elbow Left)
                { translation = vec2 upperArmLength 0
                , rotation = frame.leftElbowA
                }
                [ Node (Wrist Left)
                    { translation = vec2 lowerArmLength 0
                    , rotation = frame.leftWristA
                    }
                    []
                ]
            ]
        ]


rect tr id fillColor strokeColor cx cy w h =
    Svg.rect
        [ x <| cx - w / 2
        , y <| cy - h / 2
        , width w
        , height h
        , Svg.Events.onClick (Select id)
        , tr
        , fill fillColor
        , stroke strokeColor
        , strokeWidth 0.01
        ]
        []


tri tr id fillColor strokeColor base height offset =
    let
        -- base is AB, y = 0
        aX =
            -base / 2 |> toString

        bX =
            base / 2 |> toString

        cX =
            offset |> toString

        cY =
            height |> toString

        theD =
            [ "M", aX, "0 L", cX, cY, "L", bX, "0 Z" ] |> String.join " "
    in
    Svg.path
        [ d theD
        , Svg.Events.onClick (Select id)
        , tr
        , fill fillColor
        , stroke strokeColor
        , strokeWidth 0.01
        ]
        []


renderMantisNode : MantisBody -> Vec2 -> Float -> MantisBody -> Svg Msg
renderMantisNode selectedBodyId position angle bodyId =
    let
        tr =
            transform [ translate position, rotateRad angle ]

        isSelected =
            selectedBodyId == bodyId

        fillColor =
            "#aaa"

        strokeColor =
            if isSelected then
                "green"
            else
                "#ccc"

        rr =
            rect tr bodyId fillColor strokeColor
    in
    case bodyId of
        Torso ->
            rr 0 0 (halfTorsoWidth * 2) 0.2

        Shoulder side ->
            rr (upperArmLength / 2) 0 upperArmLength 0.1

        Elbow side ->
            rr (lowerArmLength / 2) 0 lowerArmLength 0.06

        Wrist Left ->
            tri tr bodyId fillColor strokeColor 0.05 0.3 0

        _ ->
            Debug.crash "WTF"



-- TEA


type Msg
    = Noop
    | Select MantisBody
    | MouseUp Mouse.Position
    | MouseDown Mouse.Position
    | MouseMove Mouse.Position


type alias Model =
    { frames : Array MantisFrame
    , selectedFrameIndex : Int
    , selectedNode : MantisBody
    , maybeDragStart : Maybe Mouse.Position
    , lastPosition : Mouse.Position
    }



-- init


init : Model
init =
    { frames = Array.fromList [ open1 ]
    , selectedFrameIndex = 0
    , selectedNode = Shoulder Left
    , maybeDragStart = Nothing
    , lastPosition = { x = 0, y = 0 }
    }



-- update


update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model

        Select body ->
            { model | selectedNode = body }

        MouseUp endPosition ->
            { model | maybeDragStart = Nothing }

        MouseDown startPosition ->
            { model | maybeDragStart = Just startPosition }

        MouseMove position ->
            let
                m =
                    { model | lastPosition = position }
            in
            case ( model.maybeDragStart, Array.get model.selectedFrameIndex model.frames ) of
                ( Just startPosition, Just frame ) ->
                    let
                        dx =
                            position.x - model.lastPosition.x |> toFloat

                        dy =
                            position.y - model.lastPosition.y |> toFloat

                        newFrame =
                            updateSelectedFrame ( dx, dy ) model.selectedNode frame
                    in
                    { m | frames = Array.set model.selectedFrameIndex newFrame model.frames }

                _ ->
                    m



-- view


view : Model -> Svg Msg
view model =
    case Array.get model.selectedFrameIndex model.frames of
        Nothing ->
            g [] []

        Just frame ->
            let
                unitTransform =
                    { translation = vec2 0 0
                    , rotation = 0
                    }

                nodes =
                    expandNode chainTransform unitTransform (mantisRig frame)
            in
            nodes
                |> List.map (\( { translation, rotation }, id ) -> renderMantisNode model.selectedNode translation rotation id)
                |> g [ transform [ scale 0.5 ] ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.ups MouseUp
        , Mouse.downs MouseDown
        , Mouse.moves MouseMove
        ]
