module Pages.Home_ exposing (Model, Msg, page)

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
    Layout.NavBar


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
    { title = "Conduit"
    , body = [ viewBody ]
    }


viewBody : Html msg
viewBody =
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
        , div
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
                    , div
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
                                [ Attr.href "profile.html"
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
                            ]
                        ]
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
        ]
