module Pages.Home_ exposing (Model, Msg, page)

import Api
import Api.ArticleList exposing (Article)
import Api.PopularTagsList
import Date
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes as Attr
import Http
import Iso8601 exposing (toTime)
import Layout exposing (Layout)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Time exposing (Month(..), utc)
import View exposing (View)
import Shared.Msg


layout : Layout
layout =
    Layout.HeaderAndFooter


page : Shared.Model -> Route () -> Page Model Msg
page _ _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { articleData : Api.Data (List Article)
    , popularTagData : Api.Data (List String)
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { articleData = Api.Loading, popularTagData = Api.Loading }
    , Effect.batch
        [ Api.ArticleList.getFirst20
            { onResponse = ArticleApiResponded
            }
        , Api.PopularTagsList.getTags
            { onResponse = PopularTagsApiResponded
            }
        ]
    )



-- UPDATE


type Msg
    = ArticleApiResponded (Result Http.Error (List Article))
    | PopularTagsApiResponded (Result Http.Error (List String))
    | UserClickedSignOut


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ArticleApiResponded (Ok listOfArticle) ->
            ( { model | articleData = Api.Success listOfArticle }
            , Effect.none
            )

        ArticleApiResponded (Err httpError) ->
            ( { model | articleData = Api.Failure httpError }
            , Effect.none
            )

        PopularTagsApiResponded (Ok listOfPopularTag) ->
            ( { model | popularTagData = Api.Success listOfPopularTag }
            , Effect.none
            )

        PopularTagsApiResponded (Err httpError) ->
            ( { model | articleData = Api.Failure httpError }
            , Effect.none
            )

        UserClickedSignOut ->
            ( model
            , Effect.fromSharedMsg Shared.Msg.PageSignedOutUser
            )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Conduit"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html msg
viewBody model =
    div
        [ Attr.class "home-page"
        ]
        [ div
            [ Attr.class "banner"
            ]
            [ div
                [ Attr.class "container"
                ]
                [ h1
                    [ Attr.class "logo-font"
                    ]
                    [ text "conduit" ]
                , p []
                    [ text "A place to share your knowledge." ]
                ]
            ]
        , articleView model
        ]


articleView : Model -> Html msg
articleView model =
    div
        [ Attr.class "container page"
        ]
        [ div
            [ Attr.class "row"
            ]
            [ div
                [ Attr.class "col-md-9"
                ]
                (feedView :: articleListView model)
            , popularTagView model
            ]
        ]


feedView : Html msg
feedView =
    div
        [ Attr.class "feed-toggle"
        ]
        [ ul
            [ Attr.class "nav nav-pills outline-active"
            ]
            [ li
                [ Attr.class "nav-item"
                ]
                [ a
                    [ Attr.class "nav-link disabled"
                    , Attr.href ""
                    ]
                    [ text "Your Feed" ]
                ]
            , li
                [ Attr.class "nav-item"
                ]
                [ a
                    [ Attr.class "nav-link active"
                    , Attr.href ""
                    ]
                    [ text "Global Feed" ]
                ]
            ]
        ]


popularTagView : Model -> Html msg
popularTagView model =
    div
        [ Attr.class "col-md-3"
        ]
        [ div
            [ Attr.class "sidebar"
            ]
            [ p []
                [ text "Popular Tags" ]
            , popularTagListView model
            ]
        ]


popularTagListView : Model -> Html msg
popularTagListView model =
    case model.popularTagData of
        Api.Loading ->
            div []
                [ Html.text "Loading..."
                ]

        Api.Success popularTagList ->
            div [ Attr.class "tag-list" ]
                (List.map popularTagRowView popularTagList)

        Api.Failure _ ->
            div []
                [ Html.text "Something went wrong..."
                ]


popularTagRowView : String -> Html msg
popularTagRowView tag =
    a
        [ Attr.href ""
        , Attr.class "tag-pill tag-default"
        ]
        [ text tag ]


articleListView : Model -> List (Html msg)
articleListView model =
    case model.articleData of
        Api.Loading ->
            [ div []
                [ Html.text "Loading..."
                ]
            ]

        Api.Success articleList ->
            List.map articleRowView articleList

        Api.Failure _ ->
            [ div []
                [ Html.text "Something went wrong..."
                ]
            ]


articleRowView : Article -> Html msg
articleRowView article =
    div
        [ Attr.class "article-preview"
        ]
        [ div
            [ Attr.class "article-meta"
            ]
            [ a
                [ Attr.href "profile.html"
                ]
                [ img
                    [ Attr.src article.author.image
                    ]
                    []
                ]
            , div
                [ Attr.class "info"
                ]
                [ a
                    [ Attr.href ""
                    , Attr.class "author"
                    ]
                    [ text article.author.username ]
                , span
                    [ Attr.class "date"
                    ]
                    -- [ text article.updatedAt ]
                    [ text (mydateFormat article.updatedAt) ]
                ]
            , button
                [ Attr.class "btn btn-outline-primary btn-sm pull-xs-right"
                ]
                [ i
                    [ Attr.class "ion-heart"
                    ]
                    []
                , text (String.fromInt article.favoritesCount)
                ]
            ]
        , a
            [ Attr.href ""
            , Attr.class "preview-link"
            ]
            [ h1 []
                [ text article.title ]
            , p []
                [ text article.body ]
            , span []
                [ text "Read more..." ]
            , ul [ Attr.class "tag-list" ]
                (List.map feedTagsView article.tagList)
            ]
        ]


feedTagsView : String -> Html msg
feedTagsView tags =
    li
        [ Attr.class "tag-default tag-pill tag-outline"
        ]
        [ text tags ]


mydateFormat : String -> String
mydateFormat d =
    let
        date =
            toTime d
    in
    case date of
        Ok pdate ->
            Date.format "MMMM d, y" (Date.fromPosix utc pdate)

        Err err ->
            "err"
