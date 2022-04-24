--Code adapted from http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.719.2201&rep=rep1&type=pdf
module CFG.ContextFreeGrammar exposing (..)

import Set
import Dict
import Drawable.DrawableTree as DT
import Color
import Collage
import Parser exposing (..)
import Random

type ParseTree
  = E
  | N ParseTree Char ParseTree

type alias Variable =
  Char

type alias Terminal =
  Char

type alias VarProduction =
  (Variable, (Variable, Variable))

--Chomsky Normal Form : A GFC in CNF
type alias CNF = 
  { variables : Set.Set Variable
  , productions : Set.Set VarProduction
  , terminal : Dict.Dict Terminal (List ParseTree)
  , start : Variable
  }

--Parsing the Grammar itself
charFilter : Char -> Bool
charFilter = 
  (\c -> c /= '|' && c /= ' ' && c /= '\t' && c /= '\n')

makeGrammar : String -> String -> Maybe CNF
makeGrammar prodStr startStr =
  let
    startSymbol = 
      String.toList startStr
        |> (\lst ->
              case lst of
                x::rest ->
                  Just x
                _ ->
                  Nothing
           )
    parsedProds =
      prodStr
        |> String.split "\n"
        |> List.map (run parseProd)
        |> List.concatMap 
            (\res -> 
              case res of
                Ok x ->
                  [x]
                Err _ ->
                  []
            )
    processProd cnf (lhs, lst) =
      case lhs of
        [leftVar] ->
          case lst of
            rhs::rest ->
              case String.toList rhs of
                [term] ->
                  processProd
                    { variables = Set.insert leftVar cnf.variables
                    , productions = cnf.productions
                    , terminal = 
                        case Dict.get term cnf.terminal of
                          Nothing ->
                            Dict.insert term [N (N E term E) leftVar E] cnf.terminal
                          Just xs ->
                            Dict.insert term ((N (N E term E) leftVar E)::xs) cnf.terminal
                    , start = cnf.start
                    }
                    (lhs, rest)
                [var1, var2] ->
                  processProd
                    { variables = 
                      Set.insert leftVar cnf.variables 
                        |> Set.insert var1
                        |> Set.insert var2
                    , productions = Set.insert (leftVar, (var1, var2)) cnf.productions
                    , terminal = cnf.terminal
                    , start = cnf.start
                    }
                    (lhs, rest)
                _ ->
                  processProd cnf (lhs, rest)
            [] ->
              cnf
        _ ->
          cnf
    processLines cnf lst =
      case lst of
        [] ->
          cnf
        head::rest ->
          processLines (processProd cnf head) rest
  in
   case startSymbol of
    Nothing ->
      Nothing
    Just s ->
      Just 
        (processLines {variables = Set.empty, productions = Set.empty, terminal = Dict.empty, start = s} parsedProds)

parseProd : Parser (List Variable, List String)
parseProd =
  succeed Tuple.pair
    |. spaces
    |= getChompedString (chompIf charFilter)
    |= parseTermOrVar "->"
    |> andThen (\(v, str) -> 
        (parseRHS [] str)
          |> andThen (\(resList, resStr) -> succeed (identity (String.toList v, resStr :: resList))))

parseRHS : List String -> String -> Parser (List String, String)
parseRHS lst r =
  oneOf
    [ parseTermOrVar "|"
        |> andThen (\str -> parseRHS (r :: lst) str)
    , succeed (identity (lst, r))
    ]

parseTermOrVar : String -> Parser String
parseTermOrVar opener =
  succeed identity
    |. spaces
    |. symbol opener
    |. spaces
    |= oneOf
        [ succeed (++) 
            |= backtrackable (getChompedString (chompIf charFilter))
            |. backtrackable spaces
            |= getChompedString (chompIf charFilter)
        , succeed identity 
            |= getChompedString (chompIf charFilter)
        ]
    |. spaces

--CYK Parsing
type alias Cell =
  List ParseTree

type alias Vector =
  List (Int, Cell)

member : Variable -> Cell -> Maybe ParseTree
member var cell =
  case cell of
    [] ->
      Nothing
    E::rest ->
      member var rest
    (N l v r)::rest ->
      if var == v then
        Just (N l v r)
      else
        member var rest
      

last : List a -> a
last lst =
  case List.head (List.reverse lst) of
    Nothing ->
      Debug.todo "last only works on non-empty lists"
    Just e ->
      e

--true foldl
foldl : (b -> a -> b) -> b -> List a -> b
foldl f acc lst =
  case lst of
    [] ->
      acc
    l::ls ->
      foldl f (f acc l) ls

scalarProduct : CNF -> Vector -> Vector -> Cell
scalarProduct cnf xvec yvec =
  case (xvec, yvec) of
    ([], _) ->
      []
    (_, []) ->
      []
    ((i, x)::xs, (j, y)::ys) ->
      if i < j then
        scalarProduct cnf xs yvec
      else if i > j then
        scalarProduct cnf xvec ys
      else
        (scalarProduct cnf xs ys) ++ (product cnf x y)

product : CNF -> Cell -> Cell -> Cell
product cnf c1 c2 =
  let
    multiply (c, (a,b)) =
      case (member a c1, member b c2) of
        (Nothing, _) ->
          []
        (_, Nothing) ->
          []
        (Just l, Just r) ->
          [N l c r]
  in
    cnf.productions
      |> Set.toList
      |> List.concatMap multiply

nextInputToken : CNF -> (Int, List Vector) -> Terminal -> (Int, List Vector)
nextInputToken cnf (size, vectors) token =
  let
    newSize = size + 1
    cell = 
      case Dict.get token cnf.terminal of
        Nothing -> []
        Just x -> x
    newVectors = [(newSize, cell)] :: (updateVectors cnf vectors [(size, cell)] size newSize)
  in
    (newSize, newVectors)

updateVectors : CNF -> List Vector -> Vector -> Int -> Int -> List Vector
updateVectors cnf rs col nrow ncol =
  case rs of
    [] ->
      []
    row::rows ->
      let 
        prod = scalarProduct cnf row col 
        new_nrow = nrow - 1
      in
        if List.isEmpty prod then
          row :: (updateVectors cnf rows col new_nrow ncol)
        else
          let
            new_row = row ++ [(ncol, prod)]
            new_col = (new_nrow, prod) :: col
          in
            new_row :: (updateVectors cnf rows new_col new_nrow ncol)

process : CNF -> List Terminal -> Cell
process cnf input =
  let
    (size, vectors) = foldl (nextInputToken cnf) (0, []) input
    (ncell, cell) = last (last vectors)
  in
    if size == ncell then
      cell
    else
      []

recognize : CNF -> List Terminal -> Bool
recognize cnf input =
  case member cnf.start (process cnf input) of
    Nothing ->
      False
    Just _ ->
      True

ptree : CNF -> List Terminal -> Maybe ParseTree
ptree cnf input =
  member cnf.start (process cnf input)

--Generation of random parse tree
makeRandomTree : CNF -> Random.Seed -> ParseTree
makeRandomTree cnf seed =
  let
    getGenFormVars lst dict =
      case lst of
        [] ->
          dict
        (c, (a,b))::rest ->
          case Dict.get c dict of
            Nothing ->
              getGenFormVars rest (Dict.insert c [(a,b)] dict)
            Just ls ->
              getGenFormVars rest (Dict.insert c ((a,b)::ls) dict)
    getGenFormTerms lst dict = 
      let
        getVarFromLeaf pt =
          case pt of 
            N _ v _ ->
              [v]
            _ ->
              []
        makeVarToTerms t lst1 dict1 =
          case lst1 of
            v::rest ->
              case Dict.get v dict1 of
                Nothing ->
                  makeVarToTerms t rest (Dict.insert v [t] dict1)
                Just ls ->
                  makeVarToTerms t rest (Dict.insert v (t::ls) dict1)
            [] ->
              dict1
      in
        case lst of
          [] ->
            dict
          (t, pts)::rest ->
            let vs = List.concatMap getVarFromLeaf pts in
            getGenFormTerms rest (makeVarToTerms t vs dict)
    (genVars, genTerms) = 
      ( getGenFormVars (Set.toList cnf.productions) Dict.empty
      , getGenFormTerms (Dict.toList cnf.terminal) Dict.empty
      )
    chooseNextStep s v =
      let 
        prods = 
          case Dict.get v genVars of
            Nothing -> []
            Just xs -> xs
        terms = 
          case Dict.get v genTerms of
            Nothing -> []
            Just xs -> xs
        numChoices = (List.length prods) + (List.length terms)
        (choice, newSeed) = Random.step (Random.int 1 numChoices) s
      in
        if choice <= (List.length prods) then
          case List.take 1 (List.drop (choice-1) prods) of
            [(a,b)] ->
                let (ll,lr,ns) = chooseNextStep newSeed a in
                let (rl,rr,ns1) = chooseNextStep ns b in
              (N ll a lr, N rl b rr,ns1)
            _ ->
              (E, E, newSeed)
        else
          case List.take 1 (List.drop (choice-(List.length prods)-1) terms) of
            [t] ->
              (N E t E, E, newSeed)
            _ ->
              (E, E, newSeed)
  in
    case chooseNextStep seed cnf.start of
      (l,r,_) ->
        N l (cnf.start) r

stringFromTree : ParseTree -> String
stringFromTree pt =
  let 
    listFromTree tree =
      case tree of
        E ->
          []
        N E c E ->
          [c]
        N l c r ->
          (listFromTree l) ++ (listFromTree r)
  in
    String.fromList (listFromTree pt)

  

toDrawable : ParseTree -> DT.PreDrawTree
toDrawable tree =
  case tree of
    E ->
      DT.PEmpty
    N E t E ->
      DT.PreDrawTree 
        { data = String.fromChar t
        , left = DT.PEmpty
        , right = DT.PEmpty
        , style = {fillColor = Color.lightRed, nodeShape = Collage.circle 20, centerDistance = 60}
        }
    N l v r ->
      DT.PreDrawTree 
        { data = String.fromChar v
        , left = toDrawable l
        , right = toDrawable r
        , style = {fillColor = Color.lightPurple, nodeShape = Collage.circle 20, centerDistance = 60}
        }