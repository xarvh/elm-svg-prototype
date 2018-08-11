module App exposing (..)

import Browser.Events
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Svgl.Primitives as Primitives
import Svgl.Tree exposing (..)
import WebGL exposing (Entity, Mesh, Shader)


type alias Attributes =
    { position : Vec2 }


type alias Uniforms =
    { entityToCamera : Mat4
    , dimensions : Vec2
    , fill : Vec3
    , stroke : Vec3
    , strokeWidth : Float
    , opacity : Float
    }


defaultUniforms : Uniforms
defaultUniforms =
    { entityToCamera = Mat4.identity
    , dimensions = vec2 1 1
    , fill = vec3 0.4 0.4 0.4
    , stroke = vec3 0.6 0.6 0.6
    , strokeWidth = 0.1
    , opacity = 1
    }


type alias Varying =
    { localPosition : Vec2 }


fragmentShader : Shader {} Uniforms Varying
fragmentShader =
    [glsl|
        precision mediump float;

        uniform mat4 entityToCamera;
        uniform vec2 dimensions;
        uniform vec3 fill;
        uniform vec3 stroke;
        uniform float strokeWidth;
        uniform float opacity;

        varying vec2 localPosition;

        // TODO: transform into `pixelSize`, make it a uniform
        float pixelsPerTile = 30.0;
        float e = 0.5 / pixelsPerTile;

        /*
         *     0               1                            1                     0
         *     |------|--------|----------------------------|----------|----------|
         *  -edge-e  -edge  -edge+e                      edge-e      edge      edge+e
         */
        float mirrorStep (float edge, float p) {
          return smoothstep(-edge - e, -edge + e, p) - smoothstep(edge - e, edge + e, p);
        }

        void main () {
          vec2 strokeSize = dimensions / 2.0 + strokeWidth;
          vec2 fillSize = dimensions / 2.0 - strokeWidth;

          float alpha = mirrorStep(strokeSize.x, localPosition.x) * mirrorStep(strokeSize.y, localPosition.y);
          float strokeVsFill = mirrorStep(fillSize.x, localPosition.x) * mirrorStep(fillSize.y, localPosition.y);
          vec3 color = mix(stroke, fill, strokeVsFill);

          gl_FragColor = opacity * alpha * vec4(color, 1.0);
        }
    |]


vertexShader : Shader Attributes Uniforms Varying
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec2 position;

        uniform mat4 entityToCamera;
        uniform vec2 dimensions;
        uniform vec3 fill;
        uniform vec3 stroke;
        uniform float strokeWidth;

        varying vec2 localPosition;

        void main () {
            localPosition = (dimensions + strokeWidth * 2.0) * position;
            gl_Position = entityToCamera * vec4(localPosition, 0, 1);
        }
    |]


mesh : Mesh Attributes
mesh =
    WebGL.triangles
        [ ( Attributes (vec2 -0.5 -0.5)
          , Attributes (vec2 0.5 -0.5)
          , Attributes (vec2 0.5 0.5)
          )
        , ( Attributes (vec2 -0.5 -0.5)
          , Attributes (vec2 -0.5 0.5)
          , Attributes (vec2 0.5 0.5)
          )
        ]



-- Msg


type alias Seconds =
    Float


type Msg
    = OnAnimationFrame Seconds



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
            noCmd (model + time / 1000)



-- View


view : Mat4 -> Model -> List Entity
view worldToCamera model =
    [ WebGL.entityWith Primitives.settings vertexShader fragmentShader mesh { defaultUniforms | entityToCamera = worldToCamera }
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta OnAnimationFrame



--Time.every (1000 / 10) OnAnimationFrame
