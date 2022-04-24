module Arithmetic.MathDisplay exposing (..)

import Arithmetic.ArithmeticExpressions as AE
import Drawable.DrawableTree as DT

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
  { expression : AE.Expression
  , input : String
  }

type alias Flags = 
  ()

init : Flags -> (Model, Cmd Msg)
init _ = 
  (initModel, Cmd.none)

initModel : Model
initModel =
  { expression = AE.None
  , input = ""
  }

-- UPDATE

type Msg = Reset | Parse | Input String | Step

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset -> 
      init ()
    Parse ->
      ({ expression = 
        case AE.parse model.input of
          Err _ ->
            AE.None
          Ok exp ->
            exp
      , input = model.input
      }
      , Cmd.none
      )
    Input str ->
      ({ expression = model.expression
      , input = str
      }
      , Cmd.none
      )
    Step ->
      ({ expression = 
          case AE.step model.expression of
            Nothing ->
              model.expression
            Just exp ->
              exp
      , input = model.input
      }
      , Cmd.none
      )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW

view : Model -> H.Html Msg
view model =
  H.div []
    [ H.div [] 
        [ H.h1 [Attr.style "margin-bottom" "1px"] [H.text "Arithmetic Expression Parse Trees"]
        , H.h3 [Attr.style "margin-top" "1px"] [H.text "Parsing and single-step evaluation of arithmetic expressions"]
        , H.p [Attr.style "margin-top" "0px"] [H.text instructions] 
        ] 
    , H.input [ Attr.value model.input, Events.onInput Input, Extra.onEnter Parse] []
    , H.button [ Events.onClick Parse ] [ H.text "Parse" ]
    , H.button [ Events.onClick Step ] [ H.text "Step" ]
    , H.button [ Events.onClick Reset ] [ H.text "Reset" ]
    , H.figure [] [ DT.makeCollage (DT.makeDrawable (AE.toDrawable model.expression)) |> Render.svg ]
    ]

instructions : String
instructions =
  """
  Enter any arithmetic expression using the operations +, -, *, /, ^, or %. Parentheses are supported to 
  indicate intended evaluation order. Click parse to generate a parse tree, and step to take one evaluation step.
  """