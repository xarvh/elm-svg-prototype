module App exposing (..)

import AnimationFrame
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Svg exposing (..)
import Svg.Attributes.Typed exposing (..)
import Time


--


type Node
    = Node Poly (List Node)


type alias Poly =
    { position : Vec2
    , scale : Float
    , angleToParent : Float
    , shade : Int
    , sides : Int
    }


viewPoly : Poly -> Svg a
viewPoly poly =
    regularPolygon poly.sides
        [ transform
            [ translate poly.position
            , rotateRad poly.angleToParent
            , scale poly.scale
            ]
        , fillRgb poly.shade poly.shade poly.shade
        ]


angleToVector : Float -> Vec2
angleToVector angle =
    vec2
        (sin angle)
        (cos angle)


vecToAngle : Vec2 -> Float
vecToAngle v =
    let
        ( x, y ) =
            Vec2.toTuple v
    in
    atan2 x y


normalizeAngle : Float -> Float
normalizeAngle angle =
    if angle < -pi then
        normalizeAngle (angle + 2 * pi)
    else if angle >= pi then
        normalizeAngle (angle - 2 * pi)
    else
        angle



--


makePoly : Int -> Poly -> Poly
makePoly angleIndex parent =
    let
        angleToParent =
            parent.angleToParent + toFloat angleIndex * (2 * pi / toFloat parent.sides)

        scale =
            parent.scale * 1.2

        displacement =
            Vec2.scale (scale + parent.scale) (angleToVector angleToParent)

        position =
            Vec2.add parent.position displacement
    in
    { position = position
    , scale = scale
    , angleToParent = angleToParent
    , shade = parent.shade
    , sides = parent.sides
    }


polys model =
    let
        poly0 =
            { position = vec2 0 0
            , scale = 0.1
            , angleToParent = model / 1000
            , shade = 200
            , sides = 6
            }
    in
    [ poly0
    , makePoly 0 poly0
    ]



-- Msg


type Msg
    = OnAnimationFrame Time.Time



-- Model


type alias Model =
    Float



-- Init


init : ( Model, Cmd Msg )
init =
    noCmd 0



-- Update


noCmd : Model -> ( Model, Cmd a )
noCmd model =
    ( model, Cmd.none )


update : Vec2 -> Msg -> Model -> ( Model, Cmd Msg )
update mousePosition msg model =
    case msg of
        OnAnimationFrame dt ->
            noCmd (model + dt)



-- View


checkersBackground : Int -> Svg a
checkersBackground numberOfSquaresPerSide =
    let
        squareSize =
            1.0 / toFloat numberOfSquaresPerSide

        s =
            squareSize

        s2 =
            squareSize * 2
    in
    g
        []
        [ defs
            []
            [ pattern
                [ id "grid"
                , width s2
                , height s2
                , patternUnits "userSpaceOnUse"
                ]
                [ rect
                    [ x 0
                    , y 0
                    , width s
                    , height s
                    , fill "#eee"
                    ]
                    []
                , rect
                    [ x s
                    , y s
                    , width s
                    , height s
                    , fill "#eee"
                    ]
                    []
                ]
            ]
        , rect
            [ fill "url(#grid)"
            , x -0.5
            , y -0.5
            , width 1
            , height 1
            ]
            []
        ]


view : Model -> Svg Msg
view model =
    g
        []
        [ checkersBackground 10
        , circle [ cx -0.5, cy -0.5, r 0.1, fill "red" ] []
        , circle [ cx -0.5, cy 0.5, r 0.1, fill "red" ] []
        , circle [ cx 0.5, cy 0.5, r 0.1, fill "red" ] []
        , circle [ cx 0.5, cy -0.5, r 0.1, fill "red" ] []
        , model
            |> polys
            |> List.map viewPoly
            |> g []
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs OnAnimationFrame
