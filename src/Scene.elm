module Scene exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Math.Vector4 as Vec4 exposing (Vec4, vec4)
import WebGL exposing (Entity, Mesh, Shader)


-- Shader records


type alias VertexAttributes =
    { x : Float
    , y : Float
    , vertexColor : Vec3
    }


type alias Uniforms =
    { entityToCamera : Mat4
    , time : Float
    , mousePosition : Vec2
    }


type alias Varyings =
    { color : Vec3
    , position : Vec4
    }



-- Mesh


mesh : WebGL.Mesh VertexAttributes
mesh =
    WebGL.triangles
        [ ( { x = -1
            , y = -1
            , vertexColor = vec3 1 0 0
            }
          , { x = 1
            , y = -1
            , vertexColor = vec3 0 1 0
            }
          , { x = -1
            , y = 1
            , vertexColor = vec3 0 0 1
            }
          )
        ]


vertexShader : WebGL.Shader VertexAttributes Uniforms Varyings
vertexShader =
    [glsl|
        precision mediump float;

        attribute float x;
        attribute float y;
        attribute vec3 vertexColor;

        uniform mat4 entityToCamera;
        uniform float time;
        uniform vec2 mousePosition;

        varying vec3 color;
        varying vec4 position;

        void main () {
            color = vertexColor;
            position = entityToCamera * vec4(x, y, 0, 1);
            gl_Position = position;
        }
    |]


pixelShader : WebGL.Shader {} Uniforms Varyings
pixelShader =
    [glsl|
        precision mediump float;

        uniform mat4 entityToCamera;
        uniform float time;
        uniform vec2 mousePosition;

        varying vec3 color;
        varying vec4 position;

        void main () {
          float d = distance(position.xy, mousePosition);

          float whiteness = d;

          vec3 white = vec3(1.0, 1.0, 1.0);
          vec3 blendedColor = mix(color, white, sqrt(whiteness));

          gl_FragColor = vec4(blendedColor, 1.0);
        }

    |]



-- Entities


entities { worldToCamera, mousePosition, time } =
    let
        entityToCamera =
            worldToCamera

        uniforms =
            { entityToCamera = entityToCamera
            , mousePosition = mousePosition
            , time = time
            }
    in
    [ WebGL.entity
        vertexShader
        pixelShader
        mesh
        uniforms
    ]
