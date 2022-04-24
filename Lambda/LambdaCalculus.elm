module Lambda.LambdaCalculus exposing (..)

import Parser exposing (..)
import Set

import Drawable.DrawableTree as DT
import Color
import Collage

type LExpr
  = None 
  | Var String
  | Abs String LExpr
  | App LExpr LExpr 

--Parsing

parse : String -> Result (List DeadEnd) LExpr
parse str =
  run term (String.trim str)

strVar : Parser String
strVar =
  succeed identity
    |. spaces
    |= variable
        { start = Char.isAlpha
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        }
    |. spaces

var : Parser LExpr
var = 
  strVar
    |> andThen (\str -> succeed (Var str))

varHelp : List String -> String -> Parser (List String, String)
varHelp lst v =
  oneOf
    [ strVar
      |> andThen (\str -> varHelp (v :: lst) str)
    , succeed (identity (lst, v))
    ]
  
varList : Parser (List String, String)
varList =
  strVar
    |> andThen (varHelp [])

abstraction : Parser LExpr
abstraction =
  succeed Tuple.pair
    |. spaces
    |. oneOf
        [ symbol "\\"
        , symbol "/"
        , symbol "λ"
        ]
    |. spaces
    |= varList
    |. spaces
    |. symbol "."
    |. spaces
    |= lazy (\_ -> term)
    |. spaces
    |> andThen (\((lst, v), t) -> succeed (finalizeAbstraction lst v t))

finalizeAbstraction : List String -> String -> LExpr -> LExpr
finalizeAbstraction varLst v t =
  case varLst of
    [] ->
      Abs v t
    x::rest ->
      let exp = Abs v t in
      finalizeAbstraction rest x exp

preTerm : Parser LExpr
preTerm =
  oneOf
  [ var
  , abstraction
  , succeed identity
      |. spaces
      |. symbol "("
      |. spaces
      |= lazy (\_ -> term)
      |. spaces
      |. symbol ")"
      |. spaces
  ]

term : Parser LExpr
term = 
  preTerm
    |> andThen (termHelp [])

termHelp : List LExpr -> LExpr -> Parser LExpr
termHelp exps e =
  oneOf
    [ succeed identity
        |. spaces
        |= preTerm
        |. spaces
        |> Parser.andThen (\ex -> termHelp (e :: exps) ex) 
    , lazy (\_ -> succeed (finalize exps e)) 
    ]

finalize : List LExpr -> LExpr -> LExpr
finalize exps e =
  case exps of
    [] ->
      e
    x::rest ->
      App (finalize rest x) e

unParse : LExpr -> String
unParse t =
  case t of
    Var str ->
      str
    Abs str t1 ->
      "(" ++ "λ" ++ str ++ "." ++ (unParse t1) ++ ")"
    App t1 t2 ->
      "(" ++ (unParse t1) ++ " " ++ (unParse t2) ++")"
    _ ->
      ""

--Evaluatiion

freshVar : Int -> (String, Int)
freshVar n =
  ("@" ++ (String.fromInt n), n + 1)

freeVariables : LExpr -> Set.Set String
freeVariables t =
  case t of
    Var x ->
      Set.singleton x
    Abs x t1 ->
      Set.remove x (freeVariables t1)
    App t1 t2 ->
      Set.union (freeVariables t1) (freeVariables t2)
    _ ->
      Set.empty

--replace x with t1 in t2
substitute : Int -> String -> LExpr -> LExpr -> (LExpr, Int)
substitute n x t1 t2 =
  case t2 of
    Var str ->
      if x == str then
        (t1, n)
      else
        (Var str, n)
    Abs str t3 ->
      if x == str then
        (Abs str t3, n)
      else if Set.member str (freeVariables t1) then
        let (name, newN) = freshVar n in
        let (exp, newNewN) = (substitute newN str (Var name) t3) in
        (Abs name exp, newNewN)
      else
        let (exp, newN) = (substitute n x t1 t3) in
        (Abs str exp, newN)
    App t3 t4 ->
      let (exp1, newN) = (substitute n x t1 t3) in
      let (exp2, newNewN) = (substitute newN x t1 t4) in
      (App exp1 exp2, newNewN)
    _ ->
      (None, n)

step : Int -> LExpr -> Maybe (LExpr, Int)
step n t =
  case t of
    Var _ ->
      Nothing
    Abs x t1 ->
      case step n t1 of
        Nothing ->
          Nothing
        Just (newT, newN) ->
          Just (Abs x newT, newN)
    App (Abs x t2) t1 ->
      let (exp, newN) = (substitute n x t1 t2) in
      Just (exp, newN)
    App t1 t2 ->
      case step n t1 of
        Just (exp, newN) ->
          Just (App exp t2, newN)
        Nothing ->
          case step n t2 of
            Just (exp, newN) ->
              Just (App t1 exp, newN)
            Nothing ->
              Nothing 
    _ ->
      Nothing


toDrawable : LExpr -> DT.PreDrawTree
toDrawable expr =
  case expr of
    None ->
      DT.PEmpty
    Var str ->
      DT.PreDrawTree 
        { data = str
        , left = DT.PEmpty
        , right = DT.PEmpty
        , style = {fillColor = Color.lightOrange, nodeShape = Collage.circle 20, centerDistance = 60}
        }
    Abs str t ->
      DT.PreDrawTree 
        { data = "λ"
        , left = toDrawable (Var str)
        , right = toDrawable t
        , style = {fillColor = Color.lightOrange, nodeShape = Collage.circle 20, centerDistance = 60}
        }
    App t1 t2 ->
      DT.PreDrawTree 
        { data = "$"
        , left = toDrawable t1
        , right = toDrawable t2
        , style = {fillColor = Color.lightOrange, nodeShape = Collage.circle 20, centerDistance = 60}
        }