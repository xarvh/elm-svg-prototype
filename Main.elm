module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Scene
import Task
import Time exposing (Posix)
import WebGL
import WebGL.Viewport exposing (PixelPosition, PixelSize)


-- Types


type alias Flags =
    {}


type alias Model =
    { viewportSize : PixelSize
    , mousePosition : PixelPosition
    , currentTime : Posix
    }


type Msg
    = OnResize PixelSize
    | OnMouseMove PixelPosition
    | OnAnimationFrame Posix



-- Init


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { viewportSize =
                { width = 640
                , height = 480
                }
            , mousePosition =
                { x = 0
                , y = 0
                }
            }

        cmd =
            WebGL.Viewport.getSize OnResize
    in
    ( model, cmd )



-- Update


noCmd : Model -> ( Model, Cmd Msg )
noCmd model =
    ( modle, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnResize size ->
            noCmd { model | viewportSize = size }

        OnMouseMove position ->
            noCmd { model | mousePosition = position }

        OnAnimationFrame currentTime ->
            noCmd { model | currentTime = currentTime }



-- View


view : Model -> Browser.Document Msg
view model =
    let
        scaledViewport =
            WebGL.Viewport.fitLength model.viewportSize 2.0

        entities =
            Scene.entities
                { scaledViewport = scaledViewport
                , normalizedMousePosition = Viewport.normalizedMousePosition scaledViewport model.mousePosition
                , time = model.currentTime
                }
    in
    { title = "WebGL Scaffold"
    , body = [ Viewport.canvas model.viewportSize entities ]
    }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebGL.Viewport.onResize OnResize
        , Browser.Events.onAnimationFrame OnAnimationFrame
        , Browser.Events.onMouseMove OnMouseMove
        ]



-- Main


main =
    Browser.document
        { view = view
        , subscriptions = subscriptions
        , update = update
        , init = init
        }
