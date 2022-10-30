module Pages.Profile.Username_ exposing (Model, Msg, page)

import Api
import Api.ArticleList exposing (Article)
import Auth
import Date
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes as Attr
import Http
import Iso8601 exposing (toTime)
import Json.Decode exposing (bool, maybe, string)
import Json.Decode.Pipeline exposing (required)
import Layout exposing (Layout)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Time exposing (utc)
import Url
import View exposing (View)


layout : Layout
layout =
    Layout.HeaderAndFooter


page : Auth.User -> Shared.Model -> Route { username : String } -> Page Model Msg
page user shared route =
    Page.new
        { init = init user route.params.username
        , update = update
        , subscriptions = subscriptions
        , view = view route.params.username
        }



-- INIT


type SelectedArticle
    = MyArticle
    | FavoriteArticle


type alias Model =
    { profileData : Api.Data Profile
    , articleData : Api.Data (List Article)
    , selectedFeedTab : SelectedArticle
    , isUserSignIn : Bool
    }


init : Auth.User -> String -> () -> ( Model, Effect Msg )
init mayBeUser username () =
    case mayBeUser of
        Just signinUser ->
            ( { profileData = Api.Loading
              , articleData = Api.Loading
              , selectedFeedTab = MyArticle
              , isUserSignIn = True
              }
            , Effect.batch
                [ getProfile
                    { onResponse = ProfileApiResponded
                    , username = username
                    }
                , Api.ArticleList.getFirst20ArticleBy
                    { onResponse = ArticleByAuthorApiResponded
                    , author = Url.percentDecode username
                    , favorited = Nothing
                    , token = Just signinUser.token
                    }
                ]
            )

        Nothing ->
            ( { profileData = Api.Loading
              , articleData = Api.Loading
              , selectedFeedTab = MyArticle
              , isUserSignIn = False
              }
            , Effect.batch
                [ getProfile
                    { onResponse = ProfileApiResponded
                    , username = username
                    }
                , Api.ArticleList.getFirst20ArticleBy
                    { onResponse = ArticleByAuthorApiResponded
                    , author = Url.percentDecode username
                    , favorited = Nothing
                    , token = Nothing
                    }
                ]
            )



-- UPDATE


type Msg
    = ProfileApiResponded (Result Http.Error Profile)
    | ArticleByAuthorApiResponded (Result Http.Error (List Article))


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ProfileApiResponded (Ok profile) ->
            ( { model | profileData = Api.Success profile }
            , Effect.none
            )

        ProfileApiResponded (Err httpError) ->
            ( { model | profileData = Api.Failure httpError }
            , Effect.none
            )

        ArticleByAuthorApiResponded (Err httpError) ->
            ( { model | articleData = Api.Failure httpError }
            , Effect.none
            )

        ArticleByAuthorApiResponded (Ok listOfArticle) ->
            ( { model | articleData = Api.Success listOfArticle }
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
                    (div
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
                        :: articleCardsView model.articleData
                    )
                ]
            ]
        ]


articleCardsView : Api.Data (List Article) -> List (Html msg)
articleCardsView listOfArticle =
    case listOfArticle of
        Api.Loading ->
            [ div []
                [ Html.text "Loading..."
                ]
            ]

        Api.Failure httpError ->
            [ div []
                [ Html.text (Api.ArticleList.toUserFriendlyMessage httpError)
                ]
            ]

        Api.Success articleList ->
            List.map articleCardView articleList


articleCardView : Article -> Html msg
articleCardView article =
    div
        [ Attr.class "article-preview"
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
                [ Attr.class "btn btn-outline-primary btn-sm pull-xs-right"
                ]
                [ i
                    [ Attr.class "ion-heart"
                    ]
                    []
                , text (" " ++ String.fromInt article.favoritesCount)
                ]
            ]
        , a
            [ Attr.href ("/article/" ++ article.title)
            , Attr.class "preview-link"
            ]
            [ h1 []
                [ text article.title ]
            , p []
                [ text article.body ]
            , span []
                [ text "Read more..." ]
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
