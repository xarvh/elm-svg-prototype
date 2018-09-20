module Viewport exposing (..)

import Browser.Dom
import Browser.Events
import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Task


-- Types


type alias Size length =
    { width : length
    , height : length
    }


type alias PixelSize =
    Size Int


type alias PixelPosition =
    { top : Int
    , left : Int
    }


type alias WorldPosition =
    { x : Float
    , y : Float
    }



-- Window getters


getWindowSize : (PixelSize -> msg) -> Cmd msg
getWindowSize msgConstructor =
    let
        viewportToMsg viewport =
            msgConstructor
                { width = floor viewport.viewport.width
                , height = floor viewport.viewport.height
                }
    in
    Task.perform viewportToMsg Browser.Dom.getViewport


onWindowResize : (PixelSize -> msg) -> Sub msg
onWindowResize msgConstructor =
    Browser.Events.onResize (\w h -> msgConstructor { width = w, height = h })



-- Normalized geometry


uniformScaleToFitLength : PixelSize -> Float -> Float
uniformScaleToFitLength pixelSize minimumContainedLength =
    minimumContainedLength / toFloat (min pixelSize.width pixelSize.height)


pixelToWorldUnits : PixelSize -> Float -> PixelPosition -> WorldPosition
pixelToWorldUnits pixelSize minimumContainedLength pixelPosition =
    let
        pixelX =
            pixelPosition.left - pixelSize.width // 2

        pixelY =
            1 - pixelPosition.top + pixelSize.height // 2

        scale =
            uniformScaleToFitLength pixelSize minimumContainedLength
    in
    { x = toFloat pixelX * scale
    , y = toFloat pixelY * scale
    }
      |> Debug.log "xy"


worldToPixelTransform : PixelSize -> Float -> Mat4
worldToPixelTransform pixelSize minimumContainedLength =
    let
        minSize =
            min pixelSize.width pixelSize.height

        scale =
            toFloat minSize

        scaleX =
            scale / toFloat pixelSize.width

        scaleY =
            scale / toFloat pixelSize.height
    in
    Mat4.makeScale (vec3 scaleX scaleY 1)



-- DOM element


attributes : PixelSize -> List (Attribute a)
attributes pixelSize =
    [ style "width" (String.fromInt pixelSize.width ++ "px")
    , style "height" (String.fromInt pixelSize.height ++ "px")
    , Html.Attributes.width pixelSize.width
    , Html.Attributes.height pixelSize.height
    ]
