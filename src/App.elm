module App exposing (..)

import AnimationFrame
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Svg exposing (..)
import Svg.Attributes.Typed as SAT exposing (..)
import Time


---


blade : Degrees -> Svg a
blade angle =
  let
      length = 0.4

      fillColor = "#aaa"

  in
    g
        [ transform [ rotateDeg angle ]
        ]
        [ Svg.rect
            [ width 0.025
            , height length
            , fill fillColor
            , stroke "none"
            ]
            []
        , Svg.path
            [ roundArcD length (7 * pi / 12) (5 * pi / 12)
            , fill fillColor
            , stroke "none"
            , opacity 0.5
            ]
            []
        ]


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
    let
        -- degrees per second
        speed =
            1000

        v =
            t * speed

        angleInDeg =
            toFloat (floor v % 360)
    in
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

        {-
           , g
               [ SAT.filter "url(#blur)"
               , transform [ rotateDeg angleInDeg ]
               ]
               [ propeller ]
           , circle
               [ r 0.4
               , opacity 0.2
               , SAT.filter "url(#blur)"
               ]
               []
        -}
        , blade 0

        --, roundArc 0.4 (pi /2 )
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
        OnAnimationFrame time ->
            noCmd (time / 1000)



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
    --      AnimationFrame.times OnAnimationFrame
    Time.every (1000 / 10) OnAnimationFrame
