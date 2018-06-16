module App exposing (..)

import AnimationFrame
import Ease
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Svg exposing (..)
import Svg.Attributes.Typed exposing (..)
import Svg.Events
import Time


type alias Args =
    { transformState : Float
    , lookAngle : Angle
    , fireAngle : Angle
    , fill : String
    , stroke : String
    }


strokeW : Svg.Attribute a
strokeW =
    strokeWidth 0.06


type alias RectangleArgs =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    , a : Float
    }


rectangleColor : String -> String -> RectangleArgs -> Svg a
rectangleColor fillColor strokeColor ar =
    rect
        [ transform [ translate2 ar.x ar.y, rotateDeg ar.a ]
        , fill fillColor
        , stroke strokeColor
        , strokeW
        , width ar.w
        , height ar.h
        , x (-ar.w / 2)
        , y (-ar.h / 2)
        ]
        []


mirrorRectanglesColor : String -> String -> RectangleArgs -> Svg a
mirrorRectanglesColor fillColor strokeColor ar =
    g []
        [ rectangleColor fillColor strokeColor { ar | x = -ar.x, a = -ar.a }
        , rectangleColor fillColor strokeColor ar
        ]


ellipseColor : String -> String -> { x : Float, y : Float, w : Float, h : Float } -> Svg a
ellipseColor fillColor strokeColor ar =
    Svg.ellipse
        [ cx ar.x
        , cy ar.y
        , rx (ar.w / 2)
        , ry (ar.h / 2)
        , fill fillColor
        , stroke strokeColor
        , strokeW
        ]
        []


guns : { x : Float, y : Float, w : Float, h : Float } -> Svg a
guns ar =
    mirrorRectanglesColor "#666" "#808080" { x = ar.x, y = ar.y, w = ar.w, h = ar.h, a = 0 }


eye : { x : Float, y : Float, a : Float } -> Svg a
eye ar =
    Svg.ellipse
        [ transform [ translate2 ar.x ar.y, rotateDeg ar.a ]
        , fill "#f80000"
        , stroke "#990000"
        , strokeWidth 0.03
        , ry 0.08
        , rx 0.05
        ]
        []


blimp : Args -> Svg a
blimp args =
    let
        smooth =
            view_smooth args.transformState

        step =
            view_step args.transformState 0

        rectangle =
            rectangleColor args.fill args.stroke

        mirrorRectangles =
            mirrorRectanglesColor args.fill args.stroke

        ellipse =
            ellipseColor args.fill args.stroke

        armX =
            0.6

        armW =
            0.5

        armH =
            0.9

        shdX =
            0.4

        shdY =
            -0.28

        shdH =
            0.2

        shdA =
            -30
    in
    g []
        [ g
            [ transform [ rotateRad args.fireAngle ] ]
            [ guns
                { x = smooth 0.42 0.25
                , y = smooth 0.63 0.8
                , w = smooth 0.24 0.15
                , h = 0.68
                }

            -- tail wings
            , mirrorRectangles
                { x = smooth 0.3 0.3
                , y = smooth -0.6 -0.8
                , w = smooth 0.2 0.3
                , h = 0.3
                , a = smooth 15 45
                }
            , mirrorRectangles
                { x = 0.35
                , y = smooth -0.5 -0.94
                , w = 0.35
                , h = 0.2
                , a = 0
                }

            -- side mid winglets
            , mirrorRectangles
                { x = smooth 0.4 0.5
                , y = smooth 0.5 0
                , w = smooth 0.4 0.5
                , h = 0.3
                , a = smooth 70 40
                }

            -- watermelon bottom, right arm
            , ellipse
                { x = smooth armX 0
                , y = 0
                , w = smooth armW 1.0
                , h = smooth armH 2.0
                }

            -- watermelon mid, left arm
            , ellipse
                { x = smooth -armX 0
                , y = 0
                , w = smooth armW 0.65
                , h = smooth armH 2.0
                }

            -- watermelon top, mech body
            , ellipse
                { x = 0
                , y = smooth -0.1 0
                , w = smooth 1.2 0.25
                , h = smooth 0.75 2.0
                }

            -- central tail winglet
            , rectangle
                { x = smooth shdX 0
                , y = smooth shdY -0.8
                , w = 0.1
                , h = smooth shdH 0.3
                , a = smooth shdA 0
                }
            , rectangle
                { x = smooth -shdX 0
                , y = smooth shdY -0.94
                , w = 0.1
                , h = smooth shdH 0.2
                , a = smooth -shdA 0
                }

            -- central mid winglet, head
            , rectangle
                { x = 0
                , y = 0
                , w = smooth 0.3 0.1
                , h = smooth 0.7 0.5
                , a = 0
                }
            ]
        ]


heliHead : Float -> String -> String -> Angle -> Svg a
heliHead t fillColor strokeColor angle =
    let
        smooth =
            view_smooth t

        eyeW =
            0.018 * 2

        eyeH =
            0.027 * 2

        eye xx yy aa =
            rect
                [ transform [ translate2 xx yy, rotateDeg aa ]
                , fill "#f80000"
                , stroke "#990000"
                , strokeWidth 0.01
                , width eyeW
                , height eyeH
                , x (-eyeW / 2)
                , y (-eyeH / 2)
                ]
                []
    in
    g
        [ transform [ rotateRad angle ] ]
        [ ellipse
            [ cx 0
            , cy 0.72
            , rx 0.09
            , ry 0.225
            , fill fillColor
            , stroke strokeColor
            , strokeW
            ]
            []
        , eye (smooth 0.04 0.03) (smooth 0.13 0.26) (smooth -24 -80)
        , eye (smooth -0.04 0.045) (smooth 0.13 0.21) (smooth 24 100)
        , eye (smooth 0.06 0.055) (smooth 0.05 0.16) (smooth -6 -80)
        , eye (smooth -0.06 -0.06) (smooth 0.05 0.21) (smooth 6 10)
        ]



-- Stuff from Herzog Repo


type alias Angle =
    Float


view_smooth : Float -> Float -> Float -> Float
view_smooth t a b =
    let
        tt =
            Ease.inOutCubic t
    in
    tt * b + (1 - tt) * a


view_step : Float -> Float -> Float -> Float -> Float
view_step t threshold a b =
    if t > threshold then
        b
    else
        a



-- Msg


type Msg
    = OnAnimationFrame Time.Time
    | OnTransform



-- Model


type MechOrHeli
    = Mech
    | Heli


type alias Model =
    { state : Float
    , transformTo : MechOrHeli
    , time : Float
    }



-- Init


init : ( Model, Cmd Msg )
init =
    { state = 1
    , transformTo = Heli
    , time = 0
    }
        |> noCmd



-- Update


noCmd : Model -> ( Model, Cmd a )
noCmd model =
    ( model, Cmd.none )


update : Vec2 -> Msg -> Model -> ( Model, Cmd Msg )
update mousePosition msg model =
    case msg of
        OnAnimationFrame dt ->
            let
                transformDuration =
                    1

                op =
                    case model.transformTo of
                        Mech ->
                            (-)

                        Heli ->
                            (+)
            in
            noCmd
                { model
                    | state = clamp 0 1 (op model.state (dt / 1000 / transformDuration))
                    , time = model.time + dt / 1000
                }

        OnTransform ->
            noCmd <|
                { model
                    | transformTo =
                        case model.transformTo of
                            Mech ->
                                Heli

                            Heli ->
                                Mech
                }



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


plane : Args -> Svg a
plane args =
    let
        smooth =
            view_smooth args.transformState

        step =
            view_step args.transformState 0

        rectPlate strokeColor fillColor xx yy ww hh aa =
            rect
                [ transform [ translate2 xx yy, rotateDeg aa ]
                , fill fillColor
                , stroke strokeColor
                , strokeWidth 0.02
                , width ww
                , height hh
                , x (-ww / 2)
                , y (-hh / 2)
                ]
                []

        plates xx yy ww hh aa =
            g []
                [ rectPlate args.fill args.stroke -xx yy ww hh -aa
                , rectPlate args.fill args.stroke xx yy ww hh aa
                ]
    in
    g []
        [ g
            [ transform [ scale 3, rotateRad args.fireAngle ] ]
            -- guns
            [ rectPlate "#666" "#808080" -(smooth 0.14 0.1) (smooth 0.21 0.26) (smooth 0.08 0.05) 0.26 0
            , rectPlate "#666" "#808080" (smooth 0.14 0.1) (smooth 0.21 0.26) (smooth 0.08 0.05) 0.26 0

            -- arms / front wings
            , plates
                (smooth 0.18 0.25)
                (smooth 0.1 0.03)
                (smooth 0.1 0.4)
                (smooth 0.23 0.15)
                (smooth 0 15)

            -- mid beam
            , rectPlate
                args.fill
                args.stroke
                0
                (smooth -0.04 0.04)
                (smooth 0.45 0.3)
                (smooth 0.17 0.12)
                0

            -- shoulders / rear wings
            , plates
                (smooth 0.21 0.12)
                (smooth -0.04 -0.25)
                (smooth 0.15 0.15)
                (smooth 0.23 0.25)
                (smooth 10 -45)
            ]
        , planeHead args.transformState args.fill args.stroke (step args.lookAngle args.fireAngle)
        ]


planeHead : Float -> String -> String -> Angle -> Svg a
planeHead t fillColor strokeColor angle =
    let
        smooth =
            view_smooth t

        eye xx yy aa =
            ellipse
                [ transform [ translate2 xx yy, rotateDeg aa ]
                , fill "#f80000"
                , stroke "#990000"
                , strokeWidth 0.01
                , ry 0.027
                , rx 0.018
                ]
                []
    in
    g
        [ transform [ scale 3, rotateRad angle ] ]
        [ ellipse
            [ cx 0
            , cy -0.01
            , rx 0.08
            , ry <| smooth 0.17 0.34
            , fill fillColor
            , stroke strokeColor
            , strokeWidth 0.02
            ]
            []
        , eye 0.03 (smooth 0.1 0.22) 14
        , eye -0.03 (smooth 0.1 0.22) -14
        , eye 0.05 (smooth 0.03 0.15) 6
        , eye -0.05 (smooth 0.03 0.15) -6
        ]


view : Model -> Svg Msg
view model =
    let
        args =
            { transformState = model.state
            , lookAngle = 0
            , fireAngle = 0
            , fill = "#bbb"
            , stroke = "#999"
            }
    in
    g
        [ Svg.Events.onClick OnTransform ]
        [ checkersBackground 10
        , circle [ cx -0.5, cy -0.5, r 0.1, fill "red" ] []
        , circle [ cx -0.5, cy 0.5, r 0.1, fill "red" ] []
        , circle [ cx 0.5, cy 0.5, r 0.1, fill "red" ] []
        , circle [ cx 0.5, cy -0.5, r 0.1, fill "red" ] []
        , g [ transform [ translate2 0.15 0, scale 0.3 ] ] [ blimp args ]
        , g [ transform [ scale 0.1, translate2 -4 -3 ] ] [ blimp args ]
        , g [ transform [ scale 0.1, translate2 -4 0 ] ] [ plane args ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs OnAnimationFrame
