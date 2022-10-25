module Pages.Profile.Username_ exposing (Model, Msg, page)

import Api
import Auth
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes as Attr
import Http
import Json.Decode exposing (bool, maybe, string)
import Json.Decode.Pipeline exposing (required)
import Layout exposing (Layout)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


layout : Layout
layout =
    Layout.HeaderAndFooter


page : Auth.User -> Shared.Model -> Route { username : String } -> Page Model Msg
page user shared route =
    Page.new
        { init = init route.params.username
        , update = update
        , subscriptions = subscriptions
        , view = view route.params.username
        }



-- INIT


type alias Model =
    { profileData : Api.Data Profile
    }


init : String -> () -> ( Model, Effect Msg )
init username () =
    ( { profileData = Api.Loading
      }
    , Effect.batch
        [ getProfile
            { onResponse = ProfileApiResponded
            , username = username
            }
        ]
    )



-- UPDATE


type Msg
    = ProfileApiResponded (Result Http.Error Profile)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ProfileApiResponded (Ok listOfArticle) ->
            ( { model | profileData = Api.Success listOfArticle }
            , Effect.none
            )

        ProfileApiResponded (Err httpError) ->
            ( { model | profileData = Api.Failure httpError }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : String -> Model -> View Msg
view username model =
    { title = "Pages.Profile.Username_"
    , body = [ viewBody username model ]
    }


viewBody : String -> Model -> Html Msg
viewBody username model =
    div
        [ Attr.class "profile-page"
        ]
        [ div
            [ Attr.class "user-info"
            ]
            [ div
                [ Attr.class "container"
                ]
                [ div [ Attr.class "row" ]
                    [ profileView username model ]
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


profileView : String -> Model -> Html Msg
profileView username model =
    case model.profileData of
        Api.Failure _ ->
            div
                [ Attr.class "col-xs-12 col-md-10 offset-md-1"
                ]
                []

        Api.Loading ->
            div
                [ Attr.class "col-xs-12 col-md-10 offset-md-1"
                ]
                []

        Api.Success profile ->
            div
                [ Attr.class "col-xs-12 col-md-10 offset-md-1"
                ]
                [ img
                    [ Attr.src profile.image
                    , Attr.class "user-img"
                    ]
                    []
                , h4 []
                    [ text profile.username ]
                , p []
                    [ text (Maybe.withDefault "" profile.bio) ]
                , followButon profile username
                ]


followButon : Profile -> String -> Html Msg
followButon profile username =
    if profile.username == username then
        a
            [ Attr.class "btn btn-sm btn-outline-secondary action-btn"
            , Attr.href "/settings"
            ]
            [ i
                [ Attr.class "ion-gear-a"
                ]
                []
            , text " Edit Profile Settings"
            ]

    else
        button
            [ Attr.class "btn btn-sm btn-outline-secondary action-btn"
            ]
            [ i
                [ Attr.class "ion-plus-round"
                ]
                []
            , if profile.following then
                text (" Follow " ++ profile.username)

              else
                text (" UnFollow " ++ profile.username)
            ]



--- Api


type alias Profile =
    { username : String
    , bio : Maybe String
    , image : String
    , following : Bool
    }


getProfile :
    { onResponse : Result Http.Error Profile -> msg
    , username : String
    }
    -> Effect msg
getProfile { onResponse, username } =
    Effect.fromCmd
        (Http.get
            { url = "https://api.realworld.io/api/profiles/" ++ username
            , expect = Http.expectJson onResponse profileDecoder
            }
        )


profileDecoder : Json.Decode.Decoder Profile
profileDecoder =
    Json.Decode.field "profile"
        (Json.Decode.succeed Profile
            |> required "username" string
            |> required "bio" (maybe string)
            |> required "image" string
            |> required "following" bool
        )
