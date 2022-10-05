module Pages.Article.Articletitle_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes as Attr
import Layout exposing (Layout)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


layout : Layout
layout =
    Layout.HeaderAndFooter


page : Shared.Model -> Route { articletitle : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = ExampleMsgReplaceMe


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ExampleMsgReplaceMe ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Article.Articletitle_"
    , body = [ viewBody ]
    }


viewBody : Html msg
viewBody =
    div
        [ Attr.class "article-page"
        ]
        [ div
            [ Attr.class "banner"
            ]
            [ div
                [ Attr.class "container"
                ]
                [ h1 []
                    [ text "How to build webapps that scale" ]
                , div
                    [ Attr.class "article-meta"
                    ]
                    [ a
                        [ Attr.href ""
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
                        [ Attr.class "btn btn-sm btn-outline-secondary"
                        ]
                        [ i
                            [ Attr.class "ion-plus-round"
                            ]
                            []
                        , text "Follow Eric Simons"
                        , span
                            [ Attr.class "counter"
                            ]
                            [ text "(10)" ]
                        ]
                    , button
                        [ Attr.class "btn btn-sm btn-outline-primary"
                        ]
                        [ i
                            [ Attr.class "ion-heart"
                            ]
                            []
                        , text "Favorite Post"
                        , span
                            [ Attr.class "counter"
                            ]
                            [ text "(29)" ]
                        ]
                    ]
                ]
            ]
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
                        [ text "Web development technologies have evolved at an incredible clip over the past few years." ]
                    , h2
                        [ Attr.id "introducing-ionic"
                        ]
                        [ text "Introducing RealWorld." ]
                    , p []
                        [ text "It's a great solution for learning how other frameworks work." ]
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
                        [ Attr.class "btn btn-sm btn-outline-secondary"
                        ]
                        [ i
                            [ Attr.class "ion-plus-round"
                            ]
                            []
                        , text "Follow Eric Simons"
                        ]
                    , button
                        [ Attr.class "btn btn-sm btn-outline-primary"
                        ]
                        [ i
                            [ Attr.class "ion-heart"
                            ]
                            []
                        , text "Favorite Post"
                        , span
                            [ Attr.class "counter"
                            ]
                            [ text "(29)" ]
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
