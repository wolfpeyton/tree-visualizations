module Trees.BinarySearchTree exposing (..)

import Drawable.DrawableTree as DT
import Color
import Collage

type Tree comparable
  = E
  | N (Tree comparable) comparable (Tree comparable)

empty : Tree comparable
empty =
  E

member : comparable -> Tree comparable -> Bool
member val tree =
  case tree of
    E -> 
      False
    N left nodeVal right ->
      if val == nodeVal then
        True
      else if val < nodeVal then 
        member val left
      else
        member val right

insert : comparable -> Tree comparable -> Tree comparable
insert val tree =
  case tree of
    E ->
      N E val E
    N left nodeVal right ->
      if val == nodeVal then
        tree
      else if val < nodeVal then
        N (insert val left) nodeVal right
      else
        N left nodeVal (insert val right)

insertAll : List comparable -> Tree comparable -> Tree comparable
insertAll vals tree =
  case vals of
    [] -> 
      tree
    v::vs -> 
      insertAll vals (insert v tree)

height : Tree comparable -> Int
height tree =
  case tree of
    E ->
      0
    N left _ right ->
      max (1 + (height left)) (1 + (height right))

toDrawable : Tree comparable -> DT.PreDrawTree
toDrawable tree =
  case tree of
    E ->
      DT.PEmpty
    N left a right ->
      DT.PreDrawTree 
        { data = Debug.toString a
        , left = toDrawable left
        , right = toDrawable right
        , style = {fillColor = Color.green, nodeShape = Collage.circle 20, centerDistance = 60}
        }
