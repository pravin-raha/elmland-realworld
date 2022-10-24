module Pages.Settings exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes as Attr
import Layout exposing (Layout)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)
import Auth


layout : Layout
layout =
    Layout.HeaderAndFooter


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view user
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


view :Auth.User ->  Model -> View Msg
view user model =
    { title = "Pages.Settings"
    , body = [ viewBody user]
    }


viewBody : Auth.User -> Html msg
viewBody user=
    div
        [ Attr.class "settings-page"
        ]
        [ div
            [ Attr.class "container page"
            ]
            [ div
                [ Attr.class "row"
                ]
                [ div
                    [ Attr.class "col-md-6 offset-md-3 col-xs-12"
                    ]
                    [ h1
                        [ Attr.class "text-xs-center"
                        ]
                        [ text "Your Settings" ]
                    , form []
                        [ fieldset []
                            [ fieldset
                                [ Attr.class "form-group"
                                ]
                                [ input
                                    [ Attr.class "form-control"
                                    , Attr.type_ "text"
                                    , Attr.placeholder "URL of profile picture"
                                    ]
                                    []
                                ]
                            , fieldset
                                [ Attr.class "form-group"
                                ]
                                [ input
                                    [ Attr.class "form-control form-control-lg"
                                    , Attr.type_ "text"
                                    , Attr.placeholder "Your Name"
                                    ]
                                    []
                                ]
                            , fieldset
                                [ Attr.class "form-group"
                                ]
                                [ textarea
                                    [ Attr.class "form-control form-control-lg"
                                    , Attr.rows 8
                                    , Attr.placeholder "Short bio about you"
                                    ]
                                    []
                                ]
                            , fieldset
                                [ Attr.class "form-group"
                                ]
                                [ input
                                    [ Attr.class "form-control form-control-lg"
                                    , Attr.type_ "text"
                                    , Attr.placeholder "Email"
                                    ]
                                    []
                                ]
                            , fieldset
                                [ Attr.class "form-group"
                                ]
                                [ input
                                    [ Attr.class "form-control form-control-lg"
                                    , Attr.type_ "password"
                                    , Attr.placeholder "Password"
                                    ]
                                    []
                                ]
                            , button
                                [ Attr.class "btn btn-lg btn-primary pull-xs-right"
                                ]
                                [ text "Update Settings" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
