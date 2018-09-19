module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Scene
import Time exposing (Posix)
import Viewport exposing (PixelPosition, PixelSize)
import WebGL


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
                { top = 0
                , left = 0
                }
            }

        cmd =
            Viewport.getWindowSize OnResize
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
        worldSize =
            2.0

        entities =
            Scene.entities
                { worldToCamera = Viewport.worldToPixelTransform model.viewportSize worldSize
                , mousePosition = Viewport.pixelToWorldUnits model.viewportSize worldSize model.mousePosition
                , time = model.currentTime
                }
    in
    { title = "WebGL Scaffold"
    , body = [ WebGL.toHtml (Viewport.attributes model.viewportSize) entities ]
    }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Viewport.onWindowResize OnResize
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
