module Pages.Editor exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes as Attr
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)
import Layout exposing (Layout)

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
    { title = "Pages.Editor"
    , body = [ viewBody ]
    }


viewBody : Html msg
viewBody =
    div
        [ Attr.class "editor-page"
        ]
        [ div
            [ Attr.class "container page"
            ]
            [ div
                [ Attr.class "row"
                ]
                [ div
                    [ Attr.class "col-md-10 offset-md-1 col-xs-12"
                    ]
                    [ form []
                        [ fieldset []
                            [ fieldset
                                [ Attr.class "form-group"
                                ]
                                [ input
                                    [ Attr.type_ "text"
                                    , Attr.class "form-control form-control-lg"
                                    , Attr.placeholder "Article Title"
                                    ]
                                    []
                                ]
                            , fieldset
                                [ Attr.class "form-group"
                                ]
                                [ input
                                    [ Attr.type_ "text"
                                    , Attr.class "form-control"
                                    , Attr.placeholder "What's this article about?"
                                    ]
                                    []
                                ]
                            , fieldset
                                [ Attr.class "form-group"
                                ]
                                [ textarea
                                    [ Attr.class "form-control"
                                    , Attr.rows 8
                                    , Attr.placeholder "Write your article (in markdown)"
                                    ]
                                    []
                                ]
                            , fieldset
                                [ Attr.class "form-group"
                                ]
                                [ input
                                    [ Attr.type_ "text"
                                    , Attr.class "form-control"
                                    , Attr.placeholder "Enter tags"
                                    ]
                                    []
                                , div
                                    [ Attr.class "tag-list"
                                    ]
                                    []
                                ]
                            , button
                                [ Attr.class "btn btn-lg pull-xs-right btn-primary"
                                , Attr.type_ "button"
                                ]
                                [ text "Publish Article" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
