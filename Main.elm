module Main exposing (..)

import App
import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import SplitScreen
import Task
import WebGL


type alias Flags =
    { dateNow : Int
    }


type alias Model =
    { windowSize : { width : Int, height : Int }
    , mousePosition : { x : Int, y : Int }
    , app : App.Model
    }


type Msg
    = OnWindowResizes ( Int, Int )
    | OnAppMsg App.Msg


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( appModel, appCmd ) =
            App.init

        viewportToMsg viewport =
            OnWindowResizes ( floor viewport.viewport.width, floor viewport.viewport.height )
    in
    ( { windowSize =
            { width = 1
            , height = 1
            }
      , mousePosition = { x = 0, y = 0 }
      , app = appModel
      }
    , Cmd.batch
        [ Task.perform viewportToMsg Browser.Dom.getViewport
        , appCmd |> Cmd.map OnAppMsg
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnWindowResizes ( w, h ) ->
            ( { model | windowSize = { width = w, height = h } }, Cmd.none )

        OnAppMsg nestedMsg ->
            let
                ( appModel, appCmd ) =
                    App.update (transformMousePosition model) nestedMsg model.app
            in
            ( { model | app = appModel }, Cmd.map OnAppMsg appCmd )



-- Transformations


transformMousePosition : Model -> Vec2
transformMousePosition model =
    let
        pixelW =
            toFloat model.windowSize.width

        pixelH =
            toFloat model.windowSize.height

        minSize =
            min pixelW pixelH

        pixelX =
            toFloat model.mousePosition.x - pixelW / 2

        pixelY =
            pixelH / 2 + 1 - toFloat model.mousePosition.y
    in
    vec2 (pixelX / minSize) (pixelY / minSize)


view : Model -> Browser.Document Msg
view model =
    let
        viewport =
            SplitScreen.makeViewports model.windowSize 1
                |> List.head
                |> Maybe.withDefault SplitScreen.defaultViewport

        normalizedSize =
            SplitScreen.normalizedSize viewport

        viewportScale =
            2 / SplitScreen.fitWidthAndHeight 2 2 viewport

        worldToCamera =
            Mat4.makeScale (vec3 (viewportScale / normalizedSize.width) (viewportScale / normalizedSize.height) 1)
    in
    { title = "WebGL Scaffold"
    , body =
        [ SplitScreen.viewportsWrapper
            [ App.view worldToCamera model.app
                |> WebGL.toHtmlWith [ WebGL.alpha True, WebGL.antialias ] (SplitScreen.viewportToWebGLAttributes viewport)
                |> Html.map OnAppMsg
            ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\w h -> OnWindowResizes ( w, h ))
        , App.subscriptions model.app |> Sub.map OnAppMsg
        ]


main =
    Browser.document
        { view = view
        , subscriptions = subscriptions
        , update = update
        , init = init
        }
