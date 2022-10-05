module Pages.Profile.Username_ exposing (Model, Msg, page)

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


page : Shared.Model -> Route { username : String } -> Page Model Msg
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
    { title = "Pages.Profile.Username_"
    , body = [ viewBody ]
    }


viewBody : Html Msg
viewBody =
    div
        [ Attr.class "profile-page"
        ]
        [ div
            [ Attr.class "user-info"
            ]
            [ div
                [ Attr.class "container"
                ]
                [ div
                    [ Attr.class "row"
                    ]
                    [ div
                        [ Attr.class "col-xs-12 col-md-10 offset-md-1"
                        ]
                        [ img
                            [ Attr.src "http://i.imgur.com/Qr71crq.jpg"
                            , Attr.class "user-img"
                            ]
                            []
                        , h4 []
                            [ text "Eric Simons" ]
                        , p []
                            [ text "Cofounder @GoThinkster, lived in Aol's HQ for a few months, kinda looks like Peeta from the Hunger Games" ]
                        , button
                            [ Attr.class "btn btn-sm btn-outline-secondary action-btn"
                            ]
                            [ i
                                [ Attr.class "ion-plus-round"
                                ]
                                []
                            , text "Follow Eric Simons"
                            ]
                        ]
                    ]
                ]
            ]
        , div
            [ Attr.class "container"
            ]
            [ div
                [ Attr.class "row"
                ]
                [ div
                    [ Attr.class "col-xs-12 col-md-10 offset-md-1"
                    ]
                    [ div
                        [ Attr.class "articles-toggle"
                        ]
                        [ ul
                            [ Attr.class "nav nav-pills outline-active"
                            ]
                            [ li
                                [ Attr.class "nav-item"
                                ]
                                [ a
                                    [ Attr.class "nav-link active"
                                    , Attr.href ""
                                    ]
                                    [ text "My Articles" ]
                                ]
                            , li
                                [ Attr.class "nav-item"
                                ]
                                [ a
                                    [ Attr.class "nav-link"
                                    , Attr.href ""
                                    ]
                                    [ text "Favorited Articles" ]
                                ]
                            ]
                        ]
                    , div
                        [ Attr.class "article-preview"
                        ]
                        [ div
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
                                [ text "How to build webapps that scale" ]
                            , p []
                                [ text "This is the description for the post." ]
                            , span []
                                [ text "Read more..." ]
                            ]
                        ]
                    , div
                        [ Attr.class "article-preview"
                        ]
                        [ div
                            [ Attr.class "article-meta"
                            ]
                            [ a
                                [ Attr.href ""
                                ]
                                [ img
                                    [ Attr.src "http://i.imgur.com/N4VcUeJ.jpg"
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
                                    [ text "Albert Pai" ]
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
                                , text "32"
                                ]
                            ]
                        , a
                            [ Attr.href ""
                            , Attr.class "preview-link"
                            ]
                            [ h1 []
                                [ text "The song you won't ever stop singing. No matter how hard you try." ]
                            , p []
                                [ text "This is the description for the post." ]
                            , span []
                                [ text "Read more..." ]
                            , ul
                                [ Attr.class "tag-list"
                                ]
                                [ li
                                    [ Attr.class "tag-default tag-pill tag-outline"
                                    ]
                                    [ text "Music" ]
                                , li
                                    [ Attr.class "tag-default tag-pill tag-outline"
                                    ]
                                    [ text "Song" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
