module Main exposing (main)

{-| Wraps the four visualizations in one app with a nav bar.

All four models are kept in memory at once, so each tab keeps its state when
switching away and back. The current tab lives in the URL fragment
(`#trees`, `#cfg`, ...) because GitHub Pages can't serve path-based routes.

-}

import Arithmetic.MathDisplay as Math
import Browser
import Browser.Navigation as Nav
import CFG.GrammarDisplay as Grammar
import Html as H
import Html.Attributes as Attr
import Lambda.LambdaDisplay as Lambda
import Trees.TreeDisplay as Trees
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- PAGES


type Page
    = TreesPage
    | GrammarPage
    | LambdaPage
    | MathPage


{-| Nav order. The first entry is shown when the URL has no fragment.
-}
pages : List Page
pages =
    [ GrammarPage, LambdaPage, TreesPage, MathPage ]


slug : Page -> String
slug page =
    case page of
        TreesPage ->
            "trees"

        GrammarPage ->
            "cfg"

        LambdaPage ->
            "lambda"

        MathPage ->
            "arithmetic"


label : Page -> String
label page =
    case page of
        TreesPage ->
            "Search Trees"

        GrammarPage ->
            "Context-Free Grammars"

        LambdaPage ->
            "Lambda Calculus"

        MathPage ->
            "Arithmetic"


pageFromUrl : Url -> Page
pageFromUrl url =
    pages
        |> List.filter (\page -> Just (slug page) == url.fragment)
        |> List.head
        |> Maybe.withDefault GrammarPage



-- MODEL


type alias Model =
    { key : Nav.Key
    , page : Page
    , trees : Trees.Model
    , grammar : Grammar.Model
    , lambda : Lambda.Model
    , math : Math.Model
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( trees, treesCmd ) =
            Trees.init ()

        ( grammar, grammarCmd ) =
            Grammar.init ()

        ( lambda, lambdaCmd ) =
            Lambda.init ()

        ( math, mathCmd ) =
            Math.init ()
    in
    ( { key = key
      , page = pageFromUrl url
      , trees = trees
      , grammar = grammar
      , lambda = lambda
      , math = math
      }
    , Cmd.batch
        [ Cmd.map TreesMsg treesCmd
        , Cmd.map GrammarMsg grammarCmd
        , Cmd.map LambdaMsg lambdaCmd
        , Cmd.map MathMsg mathCmd
        ]
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | TreesMsg Trees.Msg
    | GrammarMsg Grammar.Msg
    | LambdaMsg Lambda.Msg
    | MathMsg Math.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            ( { model | page = pageFromUrl url }, Cmd.none )

        TreesMsg subMsg ->
            Trees.update subMsg model.trees
                |> Tuple.mapBoth (\m -> { model | trees = m }) (Cmd.map TreesMsg)

        GrammarMsg subMsg ->
            Grammar.update subMsg model.grammar
                |> Tuple.mapBoth (\m -> { model | grammar = m }) (Cmd.map GrammarMsg)

        LambdaMsg subMsg ->
            Lambda.update subMsg model.lambda
                |> Tuple.mapBoth (\m -> { model | lambda = m }) (Cmd.map LambdaMsg)

        MathMsg subMsg ->
            Math.update subMsg model.math
                |> Tuple.mapBoth (\m -> { model | math = m }) (Cmd.map MathMsg)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = label model.page ++ " - Tree Visualizations"
    , body =
        [ H.nav [] (List.map (navLink model.page) pages)
        , H.div [ Attr.id "app" ] [ viewPage model ]
        ]
    }


navLink : Page -> Page -> H.Html Msg
navLink current page =
    H.a
        [ Attr.href ("#" ++ slug page)
        , Attr.classList [ ( "active", page == current ) ]
        ]
        [ H.text (label page) ]


viewPage : Model -> H.Html Msg
viewPage model =
    case model.page of
        TreesPage ->
            H.map TreesMsg (Trees.view model.trees)

        GrammarPage ->
            H.map GrammarMsg (Grammar.view model.grammar)

        LambdaPage ->
            H.map LambdaMsg (Lambda.view model.lambda)

        MathPage ->
            H.map MathMsg (Math.view model.math)
