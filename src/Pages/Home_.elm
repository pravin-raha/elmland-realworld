module Pages.Home_ exposing (Model, Msg, page)

import Api
import Api.ArticleList
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes as Attr
import Http
import Layout exposing (Layout)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


layout : Layout
layout =
    Layout.HeaderAndFooter


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { articleData : Api.Data (List Article)
    }


type alias Article =
    { title : String
    , body : String
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { articleData = Api.Loading }
    , Api.ArticleList.getFirst20
        { onResponse = ArticleApiResponded
        }
    )



-- UPDATE


type Msg
    = ArticleApiResponded (Result Http.Error (List Article))


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
                [ div
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
                , articleListView model
                ]
            , div
                [ Attr.class "col-md-3"
                ]
                [ div
                    [ Attr.class "sidebar"
                    ]
                    [ p []
                        [ text "Popular Tags" ]
                    , div
                        [ Attr.class "tag-list"
                        ]
                        [ a
                            [ Attr.href ""
                            , Attr.class "tag-pill tag-default"
                            ]
                            [ text "programming" ]
                        , a
                            [ Attr.href ""
                            , Attr.class "tag-pill tag-default"
                            ]
                            [ text "javascript" ]
                        , a
                            [ Attr.href ""
                            , Attr.class "tag-pill tag-default"
                            ]
                            [ text "emberjs" ]
                        , a
                            [ Attr.href ""
                            , Attr.class "tag-pill tag-default"
                            ]
                            [ text "angularjs" ]
                        , a
                            [ Attr.href ""
                            , Attr.class "tag-pill tag-default"
                            ]
                            [ text "react" ]
                        , a
                            [ Attr.href ""
                            , Attr.class "tag-pill tag-default"
                            ]
                            [ text "mean" ]
                        , a
                            [ Attr.href ""
                            , Attr.class "tag-pill tag-default"
                            ]
                            [ text "node" ]
                        , a
                            [ Attr.href ""
                            , Attr.class "tag-pill tag-default"
                            ]
                            [ text "rails" ]
                        ]
                    ]
                ]
            ]
        ]


articleListView : Model -> Html msg
articleListView model =
    case model.articleData of
        Api.Loading ->
            div []
                [ Html.text "Loading..."
                ]

        Api.Success articleList ->            
                div []
                (List.map articleRowView articleList)

        Api.Failure httpError ->
            div []
                [ Html.text "Something went wrong..."
                ]


articleRowView : Article -> Html msg
articleRowView articleList =
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
                    [ Attr.src "http://i.imgur.com/Qr71crq.jpg"
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
                    [ text "Eric Simons" ]
                , span
                    [ Attr.class "date"
                    ]
                    [ text "January 20th" ]
                ]
            , button
                [ Attr.class "btn btn-outline-primary btn-sm pull-xs-right"
                ]
                [ i
                    [ Attr.class "ion-heart"
                    ]
                    []
                , text "29"
                ]
            ]
        , a
            [ Attr.href ""
            , Attr.class "preview-link"
            ]
            [ h1 []
                [ text articleList.title ]
            , p []
                [ text articleList.body ]
            , span []
                [ text "Read more..." ]
            ]
        ]
