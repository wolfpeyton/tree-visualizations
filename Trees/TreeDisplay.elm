module Trees.TreeDisplay exposing (..)

import Trees.BinarySearchTree as B
import Trees.RedBlackTree as RBT
import Drawable.DrawableTree as DT

import Random exposing (Generator)

import Browser
import Html as H
import Html.Events as Events
import Html.Events.Extra as Extra
import Html.Attributes as Attr
import Json.Decode as Decode

import Collage.Render as Render

main : Program Flags Model Msg
main = 
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { binaryTree : B.Tree Int
  , rbTree : RBT.RBTree Int
  , prevPresent : Bool
  , input : String
  , switched : Bool
  , prevModel : PrevModel
  }

type alias PrevModel =
  { prevBTree : B.Tree Int
  , prevRBTree : RBT.RBTree Int
  }

type alias Flags = 
  ()

init : Flags -> (Model, Cmd Msg)
init _ = 
  (initModel, Cmd.none)

initModel : Model
initModel =
  { binaryTree = B.empty
  , rbTree = RBT.empty
  , prevPresent = False
  , input = ""
  , switched = False
  , prevModel =
      { prevBTree = B.empty
      , prevRBTree = RBT.empty
      }
  }

-- UPDATE

type Msg = Reset | Insert | Undo | Input String | GenRandom | RandomNum Int | Switch

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset -> 
      init ()
    Insert ->
      case String.toInt model.input of
        Nothing ->
          (model, Cmd.none)
        Just val ->
          ({ binaryTree = B.insert val model.binaryTree
          , rbTree = RBT.insert val model.rbTree
          , prevPresent = True
          , input = model.input 
          , switched = model.switched
          , prevModel = 
              { prevBTree = model.binaryTree
              , prevRBTree = model.rbTree
              }
          }
          , Cmd.none
          )
    Undo ->
      case model.prevPresent of
        False ->
          (model, Cmd.none)
        True ->
          ({ binaryTree = model.prevModel.prevBTree
          , rbTree = model.prevModel.prevRBTree
          , prevPresent = False
          , input = model.input 
          , switched = model.switched
          , prevModel = 
              { prevBTree = B.empty
              , prevRBTree = RBT.empty
              }
          }
          , Cmd.none
          )
    Input str ->
      ({ binaryTree = model.binaryTree
      , rbTree = model.rbTree
      , prevPresent = model.prevPresent
      , input = str
      , switched = model.switched
      , prevModel = model.prevModel
      }
      , Cmd.none
      )
    GenRandom ->
      (model, Random.generate RandomNum (Random.int -999 9999))
    RandomNum num ->
      let
        newModel = 
          { binaryTree = model.binaryTree
          , rbTree = model.rbTree
          , prevPresent = model.prevPresent
          , input = String.fromInt num
          , switched = model.switched
          , prevModel = model.prevModel
          }
      in
        update Insert newModel
    Switch ->
      ({ binaryTree = model.binaryTree
      , rbTree = model.rbTree
      , prevPresent = model.prevPresent
      , input = model.input
      , switched = not model.switched
      , prevModel = model.prevModel
      }
      , Cmd.none
      )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW

view : Model -> H.Html Msg
view model =
  let
    binaryTree = DT.makeCollage (DT.makeDrawable (B.toDrawable model.binaryTree)) |> Render.svg
    rbTree = DT.makeCollage (DT.makeDrawable (RBT.toDrawable model.rbTree)) |> Render.svg
  in
  H.div []
    [ H.div [] 
        [ H.h1 [Attr.style "margin-bottom" "1px"] [H.text "Binary Search Tree Visualization"]
        , H.h3 [Attr.style "margin-top" "1px"] [H.text "Red-Black Tree vs Non-Balancing Tree"] 
        ]
    , H.div []
        [ H.input [ Attr.type_ "number", Attr.value model.input, Events.onInput Input, Extra.onEnter Insert] []
        , H.button [ Events.onClick Insert ] [ H.text "Insert" ]
        , H.button [ Events.onClick Undo ] [ H.text (if model.prevPresent then "Undo" else "-") ]
        , H.button [ Events.onClick Reset ] [ H.text "Reset" ]
        , H.button [ Events.onClick GenRandom ] [ H.text "Add Random" ]
        , H.button [ Events.onClick Switch ] [ H.text "Switch" ]
        ]
    , H.hr [] []
    , if model.switched then rbTree else binaryTree
    , H.hr [] []
    , if model.switched then binaryTree else rbTree
    ]