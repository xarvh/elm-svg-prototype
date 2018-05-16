module App exposing (..)

import AnimationFrame
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Random exposing (Generator)
import Svg exposing (..)
import Svg.Attributes.Typed exposing (..)
import Time


--


type Node
    = Node Poly (List Node)


type alias Poly =
    { position : Vec2
    , size : Float
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
            , scale poly.size
            ]
        , fillRgb poly.shade poly.shade poly.shade
--         , stroke "#fff"
--         , strokeWidth 0.01
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


randomConstant : a -> Generator a
randomConstant value =
    Random.int 0 1 |> Random.map (always value)



--


type alias Options =
    { scaleF : Float -> Float
    , initialSize : Float
    , sizeLimit : Float
    , distanceF : Float -> Float -> Float
    , shadeF : Int -> Int
    , maxX : Float
    , maxY : Float
    , minX : Float
    , minY : Float
    }


opts =
    { scaleF = (+) 0.01
    , initialSize = 0.01
    , sizeLimit = 0.1
    , distanceF = (+)
    , shadeF = (+) 20
    , maxX = 0.3
    , maxY = 0.3
    , minX = -0.3
    , minY = -0.3
    }


parentMakesChild : Options -> Int -> Poly -> Poly
parentMakesChild options angleIndex parent =
    let
        angleToParent =
            parent.angleToParent + toFloat angleIndex * (2 * pi / toFloat parent.sides)

        -- parents are always *SMALLER* than children
        size =
            options.scaleF parent.size

        displacement =
            Vec2.scale (options.distanceF size parent.size) (angleToVector angleToParent)

        position =
            Vec2.add parent.position displacement

        shade =
            options.shadeF parent.shade
    in
    { position = position
    , size = size
    , angleToParent = angleToParent
    , shade = shade
    , sides = parent.sides
    }


angleToNodeGenerator : Options -> Poly -> Int -> Generator Node
angleToNodeGenerator options parent angleIndex =
    let
        child =
            parentMakesChild options angleIndex parent
    in
    if child.size > options.sizeLimit then
        randomConstant (Node child [])
    else
        generateNode options child
            |> Random.map (\new -> Node child [ new ])


generateNode : Options -> Poly -> Generator Node
generateNode options poly =
    Random.int 1 (poly.sides - 1)
        |> Random.andThen (angleToNodeGenerator options poly)


generateChain : Options -> Generator (List Poly)
generateChain options =
    let
        makePoly x y angle shade =
            { position = vec2 x y
            , size = options.initialSize
            , angleToParent = angle
            , shade = shade
            , sides = 7
            }

        append : Node -> List Poly -> List Poly
        append (Node poly childNodes) list =
            List.foldl append (poly :: list) childNodes
    in
    Random.map4 makePoly
        (Random.float options.minX options.maxX)
        (Random.float options.minY options.maxY)
        (Random.float 0 (2 * pi))
        (Random.int 0 30)
        |> Random.andThen (generateNode options)
        |> Random.map (\node -> append node [])


pol =
    let
        generator =
            Random.list 10 (generateChain opts)

        ( pl, seed ) =
            Random.step generator (Random.initialSeed 10)
    in
    pl
        |> List.concat
        |> List.sortBy .size
        |> List.reverse


polys : Model -> List Poly
polys model =
    pol



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
