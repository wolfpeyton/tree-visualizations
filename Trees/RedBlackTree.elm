-- Code is adapted from RBMaps.elm from HW5
module Trees.RedBlackTree exposing (..)

import Drawable.DrawableTree as DT
import Color
import Collage

type NodeColor = R | B

type RBTree comparable
  = E
  | T NodeColor (RBTree comparable) comparable (RBTree comparable)

empty : RBTree comparable
empty =
  E

member : comparable -> RBTree comparable -> Bool
member val rbt =
  case rbt of
    E -> 
      False
    T _ left locVal right ->
      if val < locVal then
        member val left
      else if val > locVal then
        member val right
      else
        True

insert : comparable -> RBTree comparable -> RBTree comparable
insert val rbt =
  case ins val rbt of
    T _ l v r -> T B l v r
    E -> Debug.todo "Insert: ins returned invalid tree"

ins : comparable -> RBTree comparable -> RBTree comparable
ins val rbt =
  case rbt of
    E -> T R E val E
    T c left locVal right ->
      if val == locVal then
        rbt
      else if val < locVal then
        balance c (ins val left) locVal right
      else
        balance c left locVal (ins val right)

balance : NodeColor -> RBTree comparable -> comparable -> RBTree comparable -> RBTree comparable
balance c l v r =
  case (c, (l, v, r)) of
    (B, (T R (T R ll x lr) y tr, _, _)) -> T R (T B ll x lr) y (T B tr v r)
    (B, (T R tl x (T R rl y rr), _, _)) -> T R (T B tl x rl) y (T B rr v r)
    (B, (_, _, T R (T R ll x lr) y tr)) -> T R (T B l v ll) x (T B lr y tr)
    (B, (_, _, T R tl x (T R rl y rr))) -> T R (T B l v tl) x (T B rl y rr)
    _ -> T c l v r 

toDrawable : RBTree comparable -> DT.PreDrawTree
toDrawable tree =
  case tree of
    E ->
      DT.PEmpty
    T c left a right ->
      DT.PreDrawTree 
        { data = Debug.toString a
        , left = toDrawable left
        , right = toDrawable right
        , style = 
            case c of
              R ->
                { fillColor = Color.lightRed
                , nodeShape = Collage.circle 20
                , centerDistance = 60}
              B ->
                { fillColor = Color.lightCharcoal
                , nodeShape = Collage.square 40
                , centerDistance = 60}
        }
