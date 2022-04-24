-- Code inspired by https://github.com/elm/parser/blob/master/examples/Math.elm
module Arithmetic.ArithmeticExpressions exposing (..)

import Parser exposing (..)

import Drawable.DrawableTree as DT
import Color
import Collage

type Operator
  = Plus
  | Minus
  | Times
  | DividedBy
  | RaisedTo
  | Modulo

type Expression 
  = None
  | Num Int
  | Expr Operator Expression Expression

parse : String -> Result (List DeadEnd) Expression
parse str =
  run expression (String.trim str)

digits : Parser Expression
digits =
  number
    { int = Just Num
    , hex = Nothing
    , octal = Nothing
    , binary = Nothing
    , float = Nothing
    }

term : Parser Expression
term =
  oneOf
    [ digits
    , succeed identity
        |. symbol "("
        |. spaces
        |= lazy (\_ -> expression)
        |. spaces
        |. symbol ")"
    ]

expression : Parser Expression
expression =
  term
    |> andThen (expressionHelp [])

expressionHelp : List (Expression, Operator) -> Expression -> Parser Expression
expressionHelp revOps expr =
  oneOf
    [ succeed Tuple.pair
        |. spaces
        |= operator
        |. spaces
        |= term
        |> andThen (\(op, newExpr) -> expressionHelp ((expr,op) :: revOps) newExpr)
    , lazy (\_ -> succeed (finalize revOps expr))
    ]

operator : Parser Operator
operator =
  oneOf
    [ map (\_ -> Plus) (symbol "+")
    , map (\_ -> Minus) (symbol "-")
    , map (\_ -> Times) (symbol "*")
    , map (\_ -> DividedBy) (symbol "/")
    , map (\_ -> RaisedTo) (symbol "^")
    , map (\_ -> Modulo) (symbol "%")
    ]

finalize : List (Expression, Operator) -> Expression -> Expression
finalize revOps finalExpr =
  case revOps of
    [] ->
      finalExpr
    (expr, Plus) :: rest ->
      Expr Plus (finalize rest expr) finalExpr
    (expr, Minus) :: rest ->
      Expr Minus (finalize rest expr) finalExpr
    (expr, Times) :: rest ->
      finalize rest (Expr Times expr finalExpr)
    (expr, DividedBy) :: rest ->
      finalize rest (Expr DividedBy expr finalExpr)
    (expr, RaisedTo) :: rest ->
      finalize rest (Expr RaisedTo expr finalExpr)
    (expr, Modulo) :: rest ->
      finalize rest (Expr Modulo expr finalExpr)

step : Expression -> Maybe Expression
step exp =
  case exp of
    None ->
      Nothing
    Num _ ->
      Nothing
    Expr Plus (Num n1) (Num n2) ->
      Just (Num (n1 + n2))
    Expr Minus (Num n1) (Num n2) ->
      Just (Num (n1 - n2))
    Expr Times (Num n1) (Num n2) ->
      Just (Num (n1 * n2))
    Expr DividedBy (Num n1) (Num n2) ->
      Just (Num (n1 // n2))
    Expr RaisedTo (Num n1) (Num n2) ->
      Just (Num (n1 ^ n2))
    Expr Modulo (Num n1) (Num n2) ->
      Just (Num (modBy n2 n1))
    Expr op l r ->
      case step l of
        Nothing ->
          case step r of
            Nothing ->
              Nothing
            Just rr ->
              Just (Expr op l rr)
        Just ll ->
          Just (Expr op ll r)

decodeOp : Operator -> String
decodeOp op =
  case op of
    Plus ->
      "+"
    Minus ->
      "-"
    Times ->
      "*"
    DividedBy ->
      "/"
    RaisedTo ->
      "^"
    Modulo ->
      "%"


toDrawable : Expression -> DT.PreDrawTree
toDrawable expr =
  case expr of
    None ->
      DT.PEmpty
    Num i ->
      DT.PreDrawTree 
        { data = String.fromInt i
        , left = DT.PEmpty
        , right = DT.PEmpty
        , style = {fillColor = Color.lightBlue, nodeShape = Collage.circle 20, centerDistance = 60}
        }
    Expr op left right ->
      DT.PreDrawTree 
        { data = decodeOp op
        , left = toDrawable left
        , right = toDrawable right
        , style = {fillColor = Color.lightBlue, nodeShape = Collage.circle 20, centerDistance = 60}
        }