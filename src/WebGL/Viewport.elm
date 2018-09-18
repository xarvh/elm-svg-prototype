module WebGL.Viewport exposing (..)

import Browser.Dom
import Browser.Events
import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Task


-- Viewport


type alias Viewport =
    { pixelWidth : Int
    , pixelHeight : Int
    , pixelUnitsToWorldUnits : Float
    }



-- Getter


getSize : ({ w : Int, h : Int } -> msg) -> Cmd msg
getSize msgConstructor =
    let
        viewportToMsg viewport =
            msgConstructor
                { w = floor viewport.viewport.width
                , h = floor viewport.viewport.height
                }
    in
    Task.perform viewportToMsg Browser.Dom.getViewport


onResize : ({ w : Int, h : Int } -> msg) -> Sub msg
onResize msgConstructor =
    Browser.Events.onResize (\w h -> msgConstructor { width = w, height = h })



-- Normalized geometry


{-| Scales the viewport so that the shorter dimension is guaranteed to contain
-}
fitLength : Int -> Int -> Float -> Viewport
fitLength pixelWidth pixelHeight minimumLength =
    let
        minSize =
            toFloat (min pixelWidth pixelHeight)
    in
    { pixelWidth = pixelWidth
    , pixelHeight = pixelHeight
    , pixelUnitsToWorldUnits = minimumLength / minSize
    }


mousePositionInWorldUnits : Int -> Int -> Viewport -> { x : Float, y : Float }
mousePositionInWorldUnits mouseX mouseY viewport =
    let
        viewportX =
            0

        viewportY =
            0

        pixelX =
            mouseX - viewportX - viewport.pixelWidth // 2

        pixelY =
            1 - mouseY + viewportY + viewport.pixelHeight // 2
    in
    { x = toFloat pixelX * viewport.pixelUnitsToWorldUnits
    , y = toFloat pixelY * viewport.pixelUnitsToWorldUnits
    }



-- DOM element


viewportToStyle : Viewport -> List (Attribute a)
viewportToStyle viewport =
    [ style "width" (String.fromInt viewport.pixelWidth ++ "px")
    , style "height" (String.fromInt viewport.pixelHeight ++ "px")

    --, style "position" "absolute"
    --, style "left" (String.fromInt viewport.x ++ "px")
    --, style "top" (String.fromInt viewport.y ++ "px")
    , Html.Attributes.width viewport.pixelWidth
    , Html.Attributes.height viewport.pixelHeight
    ]
