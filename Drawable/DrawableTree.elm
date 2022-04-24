-- Code inspired by https://crypto.stanford.edu/~blynn/haskell/drawtree.html
module Drawable.DrawableTree exposing (..)

import Collage
import Collage.Layout as Layout
import Collage.Text as Text
import Color


-- TYPES

type alias DrawStyle = 
  { fillColor : Color.Color
  , nodeShape : Collage.Shape
  , centerDistance : Float
  }

type PreDrawTree
  = PEmpty
  | PreDrawTree
      { data : String
      , left : PreDrawTree
      , right : PreDrawTree
      , style : DrawStyle
      }

-- Invariant for dtree: Either all children are empty, or none are
type DTree
  = DEmpty
  | DTree
      { data : Maybe String
      , left : DTree
      , right : DTree
      , style : DrawStyle
      , xpos : Float
      }

-- HELPERS

iterateStrict : Int -> (a -> a) -> a -> List a
iterateStrict num func x =
  case num of
    0 ->
      []
    1 ->
      [x]
    _ ->
      x :: (iterateStrict (num-1) func (func x))

hd : List a -> a
hd lst =
  case lst of
    [] -> 
      Debug.todo "hd/last only work for non-empty lists\n"
    a::_ -> 
      a

last : List a -> a
last =
  hd << List.reverse

zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith f la lb =
  case (la, lb) of
    (a::arest, b::brest) ->
      (f a b) :: (zipWith f arest brest)
    _ ->
      []

tupleMap : (a -> b) -> (a, a) -> (b, b)
tupleMap f (fst, snd) =
  (f fst, f snd)

height : DTree -> Int
height tree =
  case tree of
    (DTree {left, right}) ->
      case List.maximum (List.map height [left, right]) of
        Nothing -> 
          1
        Just max ->
          1 + max
    _ ->
      0

xlevels : DTree -> List (List Float)
xlevels tree =
  let
    children t =
      case t of
        (DTree {left, right}) ->
          [left, right]
        _ ->
          []
    getX t =
      case t of
        (DTree {xpos}) ->
          [xpos]
        _ ->
          []
  in
    iterateStrict (height tree) (List.concatMap children) [tree]
      |> List.map (List.concatMap getX)

xtreeMap : (Float -> Float) -> DTree -> DTree
xtreeMap f tree =
  case tree of
    (DTree {data, left, right, style, xpos}) ->
      DTree {data = data, left = xtreeMap f left, right = xtreeMap f right, xpos = f xpos, style = style}
    _ ->
      DEmpty

-- Tree handlers

makeDrawable : PreDrawTree -> DTree
makeDrawable ptree =
  case ptree of
    PEmpty ->
      DEmpty
    PreDrawTree {data, left, right, style} ->
      case (left, right) of
        (PEmpty, PEmpty) ->
          DTree {data = Just data, left = DEmpty, right = DEmpty, xpos = 0, style = style} 
        _ ->
          let
            makeEmpties tr = 
              case tr of
                PEmpty ->
                  DTree {data = Nothing, left = DEmpty, right = DEmpty, xpos = 0, style = style}
                _ ->
                  makeDrawable tr
            (ll, rr) = (makeEmpties left, makeEmpties right)
            (lcont, rcont) = (List.map hd (xlevels rr), List.map last (xlevels ll))
            d = 1 - (Maybe.withDefault -1 (List.minimum (zipWith (-) lcont rcont)))
            (newLeft, newRight) =
              if d >= 0 then
                (ll, xtreeMap ((+)d) rr)
              else
                (xtreeMap ((-)d) ll, rr)
            m = 
              case (newLeft, newRight) of
                (DTree rec1, DTree rec2) ->
                  (rec1.xpos + rec2.xpos) / 2
                _ -> 
                  0
          in
            DTree { data = Just data, left = newLeft, right = newRight, xpos = m, style = style}

-- Collage drawers

blankCollage : Collage.Collage msg
blankCollage =
  Collage.group []

drawLines : (Float, Float) -> Float -> List DTree -> Collage.Collage msg
drawLines (x,y) dist trees =
  let 
    ny = y-1
    line ts =
      case ts of 
        [] ->
          []
        DEmpty::rest ->
          line rest
        (DTree {data, xpos})::rest -> 
          case data of
            Nothing -> 
              blankCollage :: (line rest)
            Just _ ->
              (Collage.segment (x*dist,y*dist) (xpos*dist, ny*dist) |> Collage.traced (Collage.solid Collage.thin (Collage.uniform Color.black)))
              :: (line rest)
  in
    Collage.group (line trees)

makeCollage : DTree -> Collage.Collage msg
makeCollage t =
  let
    text val = 
      Text.typeface Text.Monospace <| Text.fromString val
    draw : DTree -> Float -> List (Collage.Collage msg)
    draw tree depth =
      case tree of
        (DTree {data, left, right, xpos, style}) ->
          case data of
            Nothing -> 
              [blankCollage]
            Just dat ->
              let coords = (xpos * style.centerDistance, depth * style.centerDistance) in
              [ drawLines (xpos, depth) style.centerDistance [left, right]
              , Collage.filled (Collage.uniform style.fillColor) style.nodeShape
                  |> Collage.shift coords
                  |> Layout.at Layout.base (Collage.rendered (text dat))
              , Collage.outlined Collage.defaultLineStyle style.nodeShape
                |> Collage.shift coords
              ] ++ List.concatMap (\x -> draw x (depth-1)) [left, right]
        _ ->
          [blankCollage]
  in
    Collage.group (List.reverse (draw t (toFloat (height t))))