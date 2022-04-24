module CFG.GrammarDisplay exposing (..)

import CFG.ContextFreeGrammar as CFG
import Drawable.DrawableTree as DT
import Set

import Browser
import Html as H
import Html.Events as Events
import Html.Events.Extra as Extra
import Html.Attributes as Attr
import Json.Decode as Decode

import Collage.Render as Render
import Random exposing (Generator)

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
  { tree : CFG.ParseTree
  , cnf : Maybe CFG.CNF
  , productionInput : String
  , startInput : String
  , stringInput : String
  }

type alias Flags = 
  ()

init : Flags -> (Model, Cmd Msg)
init _ = 
  (initModel, Cmd.none)

initModel : Model
initModel =
  { tree = CFG.E
  , cnf = Nothing
  , productionInput = ""
  , startInput = ""
  , stringInput = ""
  }

-- UPDATE

type Msg 
  = Reset 
  | Parse
  | ProdInput String
  | StarInput String
  | StringInput String
  | GenRandom
  | RandomSeed Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset -> 
      init ()
    Parse ->
      case CFG.makeGrammar model.productionInput model.startInput of
        Nothing -> 
          (model, Cmd.none)
        Just cnf ->
          ({ tree =
            case CFG.ptree cnf (String.toList model.stringInput) of
              Nothing ->
                CFG.E
              Just t ->
                t
          , cnf = Just cnf
          , productionInput = model.productionInput
          , startInput = model.startInput
          , stringInput = model.stringInput
          }
          , Cmd.none
          )
    ProdInput str ->
      ({ tree = model.tree
      , cnf = model.cnf
      , productionInput = str
      , startInput = model.startInput
      , stringInput = model.stringInput
      }
      , Cmd.none
      )
    StarInput str ->
      ({ tree = model.tree
      , cnf = model.cnf
      , productionInput = model.productionInput
      , startInput = str
      , stringInput = model.stringInput
      }
      , Cmd.none
      )
    StringInput str ->
      ({ tree = model.tree
      , cnf = model.cnf
      , productionInput = model.productionInput
      , startInput = model.startInput
      , stringInput = str
      }
      , Cmd.none
      )
    GenRandom ->
      (model, Random.generate RandomSeed (Random.int Random.minInt Random.maxInt))
    RandomSeed num ->
      let 
        tree = 
          case model.cnf of
            Nothing ->
              model.tree
            Just x ->
              CFG.makeRandomTree x (Random.initialSeed num)
      in
        ({ tree = tree
        , cnf = model.cnf
        , productionInput = model.productionInput
        , startInput = model.startInput
        , stringInput = CFG.stringFromTree tree
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
      [ H.h1 [Attr.style "margin-bottom" "1px"] [H.text "Context Free Grammar Parsing"]
      , H.h3 [Attr.style "margin-top" "1px"] [H.text "CYK Parsing of Grammars in Chomsky Normal Form"] 
      ]
    , H.div [] 
      [ H.h4 [Attr.style "margin-bottom" "0px"] [H.text "Productions"]
      , H.p [Attr.style "margin-top" "0px"] [H.text prodInstructions] 
      , H.textarea [Attr.cols 35, Attr.rows 10, Events.onInput ProdInput] []
      ]
    , H.div [] 
      [ H.h4 [Attr.style "margin-bottom" "0px"] [H.text "Start Variable"]
      , H.p [Attr.style "margin-top" "0px"] [H.text startInstructions] 
      , H.input [ Attr.value model.startInput, Events.onInput StarInput] []
      ]
    , H.div [] 
      [ H.h4 [Attr.style "margin-bottom" "0px"] [H.text "String to Parse"]
      , H.p [Attr.style "margin-top" "0px"] [H.text parseInstructions] 
      , H.input [ Attr.value model.stringInput, Events.onInput StringInput, Extra.onEnter Parse] []
      ]
    , H.button [ Events.onClick Parse ] [ H.text "Parse" ]
    , H.button [ Events.onClick Reset ] [ H.text "Reset" ]
    , H.button [ Events.onClick GenRandom ] [ H.text "Random" ]
    , H.div [] [H.text model.stringInput]
    , H.figure [] [ DT.makeCollage (DT.makeDrawable (CFG.toDrawable model.tree)) |> Render.svg ]
    ]

prodInstructions : String
prodInstructions =
  """
  Enter the productions of your grammar. Those should look like "V -> t | AB", where V, A, and B are variables 
  in your grammar, and t is a terminal. You can have as many of these as you want from an individual variable. 
  For each new variable's production rules, go to a new line. Separate one variable's rules with a "|". Since 
  the grammar must be in chomsky normal form, the right hand side of your rules can only contain single terminals 
  or pairs of variables. Whitespace is ignored. 
  """

startInstructions : String
startInstructions =
  """
  Enter the start variable of your grammar. This must be a single character, and it must be one of your variables.
  """

parseInstructions : String
parseInstructions =
  """
  Enter the string you want to parse with this grammar. If it's a member of the language described by the grammar, 
  a parse tree for the string will be drawn.
  """