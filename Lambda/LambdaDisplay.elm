module Lambda.LambdaDisplay exposing (..)

import Lambda.LambdaCalculus as LC
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
  { expression : LC.LExpr
  , input : String
  , variableNamer : Int
  }

type alias Flags = 
  ()

init : Flags -> (Model, Cmd Msg)
init _ = 
  (initModel, Cmd.none)

initModel : Model
initModel =
  { expression = LC.None
  , input = ""
  , variableNamer = 0
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
        case LC.parse model.input of
          Err _ ->
            LC.None
          Ok exp ->
            exp
      , input = model.input
      , variableNamer = model.variableNamer
      }
      , Cmd.none
      )
    Input str ->
      ({ expression = model.expression
      , input = str
      , variableNamer = model.variableNamer
      }
      , Cmd.none
      )
    Step ->
      case LC.step model.variableNamer model.expression of
        Nothing ->
          (model, Cmd.none)
        Just (exp, newN) ->  
          ({ expression = exp
          , input = model.input
          , variableNamer = newN
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
        [ H.h1 [Attr.style "margin-bottom" "1px"] [H.text "Lambda Calculus Parse Trees"]
        , H.h3 [Attr.style "margin-top" "1px"] [H.text "Parsing and single-step evaluation of the untyped lambda calculus"]
        , H.p [Attr.style "margin-top" "0px"] [H.text instructions] 
        , H.p [Attr.style "margin-top" "0px"] [H.text details ] 
        ] 
    , H.input [ Attr.value model.input, Events.onInput Input, Extra.onEnter Parse] []
    , H.button [ Events.onClick Parse ] [ H.text "Parse" ]
    , H.button [ Events.onClick Step ] [ H.text "Step" ]
    , H.button [ Events.onClick Reset ] [ H.text "Reset" ]
    , H.div [] [ H.text (LC.unParse model.expression) ]
    , H.figure [] [ DT.makeCollage (DT.makeDrawable (LC.toDrawable model.expression)) |> Render.svg ]
    ]

instructions : String
instructions =
  """
  The untyped lambda calculus is a language capable of computing anything that can be computed. 
  A term T in the language can be a variable (x), an abstraction (λx.T), or an application (T T). 
  Multicharacter variables are supported, so whitespace or parentheses must be used to separate 
  different variables. In addition, syntactic sugar for nested abstractions is supported: (λx y.x) 
  is equivalent to (λx.λy.x). In addition to λ, the parser also accepts / or \\.
  """

details : String
details =
  """
  Abstraction binds as tightly as possible, so (λx.x y) is equivalent to (λx.(x y)). Application is 
  left associative, so (x y z) is equivalent to ((x y) z). The evaluator uses a full beta reduction strategy, 
  and will also alpha reduce (rename) variables as necessary. In such cases, a variable like @0 may appear. 
  Although the string may be different, it's guaranteed to be alpha equivalent to a correct evaluation. 
  """