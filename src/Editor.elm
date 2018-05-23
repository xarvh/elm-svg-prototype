module Editor exposing (..)

import Array exposing (Array)
import Html
import Keyboard
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
    | UpperArm Side
    | LowerArm Side
    | Blade Side


type alias MantisNode =
    Node Transform MantisBody


type alias Rect =
    { angle : Float
    , length : Float
    , width : Float
    }


type RectTarget
    = Angle
    | Width
    | Length


updateRect : Float -> RectTarget -> Rect -> Rect
updateRect delta target rect =
    case target of
        Angle ->
            { rect | angle = rect.angle + 0.005 * delta }

        Width ->
            { rect | width = rect.width + 0.01 * delta |> max 0.01 }

        Length ->
            { rect | length = rect.length + 0.01 * delta |> max 0.01 }


type alias MantisFrame =
    { torso : Rect
    , leftShoulder : Rect
    , leftUpperArm : Rect
    , leftLowerArm : Rect
    , leftBlade : Rect
    }


open1 : MantisFrame
open1 =
    { torso = { angle = 0, width = 0.2, length = 0.1 }
    , leftShoulder = { angle = 0, width = 0.3, length = 0.1 }
    , leftUpperArm = { angle = 0, width = 0.2, length = 0.1 }
    , leftLowerArm = { angle = 0, width = 0.2, length = 0.1 }
    , leftBlade = { angle = 0, width = 0.1, length = 0.2 }
    }


updateSelectedFrame : Bool -> Bool -> ( Float, Float ) -> MantisBody -> MantisFrame -> MantisFrame
updateSelectedFrame shift ctrl ( dx, dy ) nodeId frame =
    let
        target =
            if ctrl then
                Length
            else if shift then
                Width
            else
                Angle
    in
    case nodeId of
        Torso ->
            { frame | torso = updateRect dx target frame.torso }

        Shoulder Left ->
            { frame | leftShoulder = updateRect dx target frame.leftShoulder }

        UpperArm Left ->
            { frame | leftUpperArm = updateRect dx target frame.leftUpperArm }

        LowerArm Left ->
            { frame | leftLowerArm = updateRect dx target frame.leftLowerArm }

        Blade Left ->
            { frame | leftBlade = updateRect dx target frame.leftBlade }

        _ ->
            frame



--


mantisRig : MantisFrame -> MantisNode
mantisRig frame =
    Node Torso
        { translation = vec2 0 0
        , rotation = frame.torso.angle
        }
        [ Node (Shoulder Left)
            { translation = vec2 -(frame.torso.width / 2) 0
            , rotation = frame.leftShoulder.angle
            }
            [ Node (UpperArm Left)
                { translation = vec2 frame.leftShoulder.length 0
                , rotation = frame.leftUpperArm.angle
                }
                [ Node (LowerArm Left)
                    { translation = vec2 frame.leftUpperArm.length 0
                    , rotation = frame.leftLowerArm.angle
                    }
                    [ Node (Blade Left)
                        { translation = vec2 frame.leftLowerArm.length 0
                        , rotation = frame.leftBlade.angle
                        }
                        []
                    ]
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


renderMantisNode : MantisFrame -> MantisBody -> Vec2 -> Float -> MantisBody -> Svg Msg
renderMantisNode frame selectedBodyId position angle bodyId =
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
            rr 0 0 frame.torso.width frame.torso.length

        Shoulder side ->
            rr (frame.leftShoulder.length / 2) 0 frame.leftShoulder.length frame.leftShoulder.width

        UpperArm side ->
            rr (frame.leftUpperArm.length / 2) 0 frame.leftUpperArm.length frame.leftUpperArm.width

        LowerArm side ->
            rr (frame.leftLowerArm.length / 2) 0 frame.leftLowerArm.length frame.leftLowerArm.width

        Blade Left ->
            tri tr bodyId fillColor strokeColor frame.leftBlade.length frame.leftBlade.width 0

        _ ->
            Debug.crash "WTF"



-- TEA


type Msg
    = Noop
    | Select MantisBody
    | MouseUp Mouse.Position
    | MouseDown Mouse.Position
    | MouseMove Mouse.Position
    | Keyboard Bool Int


type alias Model =
    { frames : Array MantisFrame
    , selectedFrameIndex : Int
    , selectedNode : MantisBody
    , maybeDragStart : Maybe Mouse.Position
    , lastPosition : Mouse.Position
    , isShiftPressed : Bool
    , isCtrlPressed : Bool
    }



-- init


init : Model
init =
    { frames = Array.fromList [ open1 ]
    , selectedFrameIndex = 0
    , selectedNode = Shoulder Left
    , maybeDragStart = Nothing
    , lastPosition = { x = 0, y = 0 }
    , isShiftPressed = False
    , isCtrlPressed = False
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
                            updateSelectedFrame model.isShiftPressed model.isCtrlPressed ( dx, dy ) model.selectedNode frame
                    in
                    { m | frames = Array.set model.selectedFrameIndex newFrame model.frames }

                _ ->
                    m

        Keyboard state key ->
            case key of
                16 ->
                    { model | isShiftPressed = state }

                17 ->
                    { model | isCtrlPressed = state }

                _ ->
                    model



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
                |> List.map (\( { translation, rotation }, id ) -> renderMantisNode frame model.selectedNode translation rotation id)
                |> g [ transform [ scale 0.5 ] ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.ups MouseUp
        , Mouse.downs MouseDown
        , Mouse.moves MouseMove
        , Keyboard.downs (Keyboard True)
        , Keyboard.ups (Keyboard False)
        ]
