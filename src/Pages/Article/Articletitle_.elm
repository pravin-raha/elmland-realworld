module Pages.Article.Articletitle_ exposing (Model, Msg, page)

import Api exposing (Data(..))
import Api.ArticleList exposing (Article)
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
import Time exposing (utc)
import View exposing (View)


layout : Layout
layout =
    Layout.HeaderAndFooter


page : Shared.Model -> Route { articletitle : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init route.params.articletitle
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { articleData : Api.Data Article
    , slug : String
    }


init : String -> () -> ( Model, Effect Msg )
init slug () =
    ( { articleData = Api.Loading
      , slug = slug
      }
    , Effect.batch
        [ Api.ArticleList.getArticle
            { onResponse = ArticleApiResponded
            , token = Nothing
            , slug = slug
            }
        ]
    )



-- UPDATE


type Msg
    = ArticleApiResponded (Result Http.Error Article)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ArticleApiResponded (Ok article) ->
            ( { model | articleData = Api.Success article }
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
    { title = model.slug
    , body = [ viewBody model ]
    }


viewBody : Model -> Html msg
viewBody model =
    case model.articleData of
        Api.Loading ->
            div []
                [ Html.text "Loading..."
                ]

        Api.Success article ->
            div
                [ Attr.class "article-page"
                ]
                [ titleView article
                , div
                    [ Attr.class "container page"
                    ]
                    [ div
                        [ Attr.class "row article-content"
                        ]
                        [ div
                            [ Attr.class "col-md-12"
                            ]
                            [ p []
                                [ text article.title ]
                            , h2
                                [ Attr.id "introducing-ionic"
                                ]
                                [ text article.description ]
                            , p []
                                [ text article.body ]
                            ]
                        ]
                    , hr []
                        []
                    , div
                        [ Attr.class "article-actions"
                        ]
                        [ div
                            [ Attr.class "article-meta"
                            ]
                            [ a
                                [ Attr.href ("/profile/" ++ article.author.username)
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
                                    [ text (mydateFormat article.updatedAt) ]
                                ]
                            , button
                                [ Attr.class "btn btn-sm btn-outline-secondary"
                                ]
                                [ i
                                    [ Attr.class "ion-plus-round"
                                    ]
                                    []
                                , text (" Follow " ++ article.author.username)
                                ]
                            , button
                                [ Attr.class "btn btn-sm btn-outline-primary"
                                ]
                                [ i
                                    [ Attr.class "ion-heart"
                                    ]
                                    []
                                , text " Favorite Post"
                                , span
                                    [ Attr.class "counter"
                                    ]
                                    [ text ("(" ++ String.fromInt article.favoritesCount ++ ")") ]
                                ]
                            ]
                        ]
                    , div
                        [ Attr.class "row"
                        ]
                        [ div
                            [ Attr.class "col-xs-12 col-md-8 offset-md-2"
                            ]
                            [ form
                                [ Attr.class "card comment-form"
                                ]
                                [ div
                                    [ Attr.class "card-block"
                                    ]
                                    [ textarea
                                        [ Attr.class "form-control"
                                        , Attr.placeholder "Write a comment..."
                                        , Attr.rows 3
                                        ]
                                        []
                                    ]
                                , div
                                    [ Attr.class "card-footer"
                                    ]
                                    [ img
                                        [ Attr.src "http://i.imgur.com/Qr71crq.jpg"
                                        , Attr.class "comment-author-img"
                                        ]
                                        []
                                    , button
                                        [ Attr.class "btn btn-sm btn-primary"
                                        ]
                                        [ text "Post Comment" ]
                                    ]
                                ]
                            , div
                                [ Attr.class "card"
                                ]
                                [ div
                                    [ Attr.class "card-block"
                                    ]
                                    [ p
                                        [ Attr.class "card-text"
                                        ]
                                        [ text "With supporting text below as a natural lead-in to additional content." ]
                                    ]
                                , div
                                    [ Attr.class "card-footer"
                                    ]
                                    [ a
                                        [ Attr.href ""
                                        , Attr.class "comment-author"
                                        ]
                                        [ img
                                            [ Attr.src "http://i.imgur.com/Qr71crq.jpg"
                                            , Attr.class "comment-author-img"
                                            ]
                                            []
                                        ]
                                    , a
                                        [ Attr.href ""
                                        , Attr.class "comment-author"
                                        ]
                                        [ text "Jacob Schmidt" ]
                                    , span
                                        [ Attr.class "date-posted"
                                        ]
                                        [ text "Dec 29th" ]
                                    ]
                                ]
                            , div
                                [ Attr.class "card"
                                ]
                                [ div
                                    [ Attr.class "card-block"
                                    ]
                                    [ p
                                        [ Attr.class "card-text"
                                        ]
                                        [ text "With supporting text below as a natural lead-in to additional content." ]
                                    ]
                                , div
                                    [ Attr.class "card-footer"
                                    ]
                                    [ a
                                        [ Attr.href ""
                                        , Attr.class "comment-author"
                                        ]
                                        [ img
                                            [ Attr.src "http://i.imgur.com/Qr71crq.jpg"
                                            , Attr.class "comment-author-img"
                                            ]
                                            []
                                        ]
                                    , a
                                        [ Attr.href ""
                                        , Attr.class "comment-author"
                                        ]
                                        [ text "Jacob Schmidt" ]
                                    , span
                                        [ Attr.class "date-posted"
                                        ]
                                        [ text "Dec 29th" ]
                                    , span
                                        [ Attr.class "mod-options"
                                        ]
                                        [ i
                                            [ Attr.class "ion-edit"
                                            ]
                                            []
                                        , i
                                            [ Attr.class "ion-trash-a"
                                            ]
                                            []
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]

        Failure httpError ->
            div []
                [ Html.text (Api.ArticleList.toUserFriendlyMessage httpError)
                ]


titleView : Article -> Html msg
titleView article =
    div [ Attr.class "banner" ]
        [ div
            [ Attr.class "container"
            ]
            [ h1 []
                [ text article.title ]
            , div
                [ Attr.class "article-meta"
                ]
                [ a
                    [ Attr.href ("/profile/" ++ article.author.username)
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
                        [ Attr.href ("/profile/" ++ article.author.username)
                        , Attr.class "author"
                        ]
                        [ text article.author.username ]
                    , span
                        [ Attr.class "date"
                        ]
                        [ text (mydateFormat article.updatedAt) ]
                    ]
                , button
                    [ Attr.class "btn btn-sm btn-outline-secondary"
                    ]
                    [ i
                        [ Attr.class "ion-plus-round"
                        ]
                        []
                    , text (" Follow " ++ article.author.username)
                    ]
                , button
                    [ Attr.class "btn btn-sm btn-outline-primary"
                    ]
                    [ i
                        [ Attr.class "ion-heart"
                        ]
                        []
                    , text " Favorite Post"
                    , span
                        [ Attr.class "counter"
                        ]
                        [ text ("(" ++ String.fromInt article.favoritesCount ++ ")") ]
                    ]
                ]
            ]
        ]


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
