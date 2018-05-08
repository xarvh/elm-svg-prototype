module App exposing (..)

import AnimationFrame
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Svg exposing (..)
import Svg.Attributes.Typed as SAT exposing (..)
import Time


---


blade : Degrees -> Svg a
blade angle =
    Svg.rect
        [ width 0.025
        , height 0.4
        , fill "#aaa"
        , stroke "none"
        , transform [ rotateDeg angle ]
        ]
        []


propeller : Svg a
propeller =
    g
        []
        [ blade 0
        , blade 120
        , blade 240
        ]


prop : Seconds -> Svg a
prop t =
    g
        []
        [ defs
            []
            [ Svg.filter
                [ id "blur" ]
                [ feGaussianBlur
                    [ result "blur"
                    , stdDeviation "0.008 0"
                    ]
                    []
                ]
            ]
        , g
            [ SAT.filter "url(#blur)"
            , transform [ rotateDeg (t * -200) ]
            ]
            [ propeller ]
        , g
            [ SAT.filter "url(#blur)"
            , opacity 0.5
            , transform [ rotateDeg (120 + t * -333) ]
            ]
            [ propeller ]
        , g
            [ SAT.filter "url(#blur)"
            , transform [ rotateDeg (240 + t * -777) ]
            , opacity 0.3
            ]
            [ propeller ]
        ]



---


type alias Seconds =
    Float



-- Msg


type Msg
    = OnAnimationFrame Time.Time



-- Model


type alias Model =
    Seconds



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
            noCmd (model + dt / 1000)



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
        , prop model
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    --AnimationFrame.diffs OnAnimationFrame
    Time.every (1000 / 10) OnAnimationFrame
