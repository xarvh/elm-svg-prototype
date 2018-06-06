module App exposing (..)

import AnimationFrame
import Ease
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Svg exposing (..)
import Svg.Attributes.Typed exposing (..)
import Svg.Events
import Time


mech : Float -> Angle -> Angle -> String -> String -> Svg a
mech t headAngle topAngle darkColor brightColor =
    let
        smooth =
            view_smooth t

        step =
            view_step t 0

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
                [ rectPlate brightColor darkColor -xx yy ww hh -aa
                , rectPlate brightColor darkColor xx yy ww hh aa
                ]
    in
    g []
        [ g
            [ transform [ scale 3, rotateRad topAngle ] ]
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
                brightColor
                darkColor
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
        , head t brightColor darkColor (step headAngle topAngle)
        ]


head : Float -> String -> String -> Angle -> Svg a
head t brightColor darkColor angle =
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
        [ transform [ scale 3, rotateRad angle ] ]
        [ ellipse
            [ cx 0
            , cy <| smooth -0.01 0.11
            , rx <| smooth 0.08 0.10
            , ry  0.17
            , fill darkColor
            , stroke brightColor
            , strokeWidth 0.02
            ]
            []
        , eye (smooth 0.05 0.06) (smooth 0.13 0.25) -14
        , eye (smooth -0.05 -0.06) (smooth 0.13 0.25) 14
        , eye (smooth 0.07 0.08) (smooth 0.05 0.17) -6
        , eye (smooth -0.07 -0.08) (smooth 0.05 0.17) 6
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
    }



-- Init


init : ( Model, Cmd Msg )
init =
    { state = 0
    , transformTo = Mech
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
            noCmd { model | state = clamp 0 1 (op model.state (dt / 1000 / transformDuration)) }

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


view : Model -> Svg Msg
view model =
    g
        [ Svg.Events.onClick OnTransform ]
        [ checkersBackground 10
        , circle [ cx -0.5, cy -0.5, r 0.1, fill "red" ] []
        , circle [ cx -0.5, cy 0.5, r 0.1, fill "red" ] []
        , circle [ cx 0.5, cy 0.5, r 0.1, fill "red" ] []
        , circle [ cx 0.5, cy -0.5, r 0.1, fill "red" ] []
        , g
            [ transform [ scale 0.3 ]
            ]
            [ mech model.state 0 0 "#bbb" "#999" ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs OnAnimationFrame
