module Editor exposing (..)

import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Svg exposing (Svg, g)
import Svg.Attributes.Typed exposing (..)


origin =
    vec2 0 0



-- Nodes


type Node id frame
    = Node
      { id : id
      , origin : frame -> Vec2
      , rotation : frame -> Float
      }
      (List (Node id frame))



drawRig : Vec2 -> Float -> (Vec2 -> Float -> Id -> renderArtefact) -> frame -> Node id frame -> List renderArtefact
drawRig parentAbsoluteOrigin parentAbsoluteRotation renderNode frame (Node node children) =
  let
      absoluteRotation = parentAbsoluteRotation + (node.rotation frame)

      offset = node.origin frame |> rotate absoluteRotation



      absoluteOrigin = Vec2.add parentAbsoluteOrigin offset

      drawChildren =
        drawRig absoluteOrigin absoluteRotation renderNode frame

  in
  renderNode absoluteOrigin absoluteRotation :: List.map drawChildren children






-- mantis rig

type Side
  = Left | Right

type Node id frame
    = Node
      { id : id
      , origin : frame -> Vec2
      , rotation : frame -> Float
      }
      (List (Node id frame))



drawRig : Vec2 -> Float -> (Vec2 -> Float -> Id -> renderArtefact) -> frame -> Node id frame -> List renderArtefact
drawRig parentAbsoluteOrigin parentAbsoluteRotation renderNode frame (Node node children) =
  let
      absoluteOrigin = Vec2.add parentAbsoluteOrigin (node.origin frame)
      absoluteRotation = parentAbsoluteRotation + (node.rotation frame)

      drawChildren =
        drawRig absoluteOrigin absoluteRotation renderNode frame

  in
  renderNode absoluteOrigin absoluteRotation :: List.map drawChildren children






-- mantis rig

type Side
  = Left | Right


type MantisBody
  = Torso
  | Shoulder Side
  | Elbow Side
  | Wrist Side

type alias MantisFrame =
    { torsoA : Float
    , leftShoulderA : Float
    , leftElbowA : Float
    }


mantisRig : Node MantisBody MantisFrame
mantisRig =
    Node
      { id = Torso
      , origin = always (vec2 0 0)
      , rotation = .torsoA
      }
      [ Node
        { id = Shoulder Left
        , origin = leftShoulderOrigin
        , rotation = .leftShoulderA
        }
        [ Node
          { id = Elbow Left
          , origin = leftElbowOrigin
          , rotation = .leftElbowA
          }
          []
        ]
      ]


-- draw


-- TEA


type Msg
    = Noop


type alias Model =
    {}



-- init


init : Model
init =
    {}



-- update


update : Msg -> Model -> Model
update msg model =
    model



-- view


view : Model -> Svg Msg
view model =
    Svg.g [] []



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
