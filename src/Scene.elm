module Scene exposing (..)

import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL exposing (Entity, Mesh, Shader)


-- Shader records


type alias VertexAttributes =
    { x : Float
    , y : Float
    , vertexColor : Vec3
    }


type alias Uniforms =
    { color : Vec3
    , time : Float
    , mousePosition : Vec2
    }


type alias Varyings =
    { varyingColor : Vec3
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

        uniform vec3 color;
        uniform float time;

        varying vec3 varyingColor;

        void main () {
            varyingColor = vertexColor;
            gl_Position.x = x;
            gl_Position.y = y;
            gl_Position.z = 0.0;
            gl_Position.w = 2.0;
        }
    |]


pixelShader : WebGL.Shader {} Uniforms Varyings
pixelShader =
    [glsl|
        precision mediump float;

        uniform vec3 color;
        uniform float time;

        varying vec3 varyingColor;

        void main () {
          gl_FragColor = vec4(varyingColor, 1.0);
        }

    |]



-- Entities


entities { worldToCamera, mousePosition, time } =
    let
        entityToCamera =
            worldToCamera
                |> Mat4.scale 1

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
