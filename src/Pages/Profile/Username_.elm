module Pages.Profile.Username_ exposing (Model, Msg, page)

import Api
import Api.Article exposing (Article)
import Api.Profile exposing (Profile)
import Auth
import Date
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http
import Iso8601 exposing (toTime)
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
page user _ route =
    Page.new
        { init = init user (Maybe.withDefault "" (Url.percentDecode route.params.username))
        , update = update (Maybe.withDefault "" (Url.percentDecode route.params.username))
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
    , isFavoriteButtonClicked : Bool
    , token : Maybe String
    , username : String
    , isFollowedButtonClicked : Bool
    }


init : Auth.User -> String -> () -> ( Model, Effect Msg )
init mayBeUser username () =
    case mayBeUser of
        Just signinUser ->
            ( { profileData = Api.Loading
              , articleData = Api.Loading
              , selectedFeedTab = MyArticle
              , isUserSignIn = True
              , isFavoriteButtonClicked = False
              , token = Maybe.map (\u -> u.token) mayBeUser
              , username = username
              , isFollowedButtonClicked = False
              }
            , Effect.batch
                [ Api.Profile.getProfile
                    { onResponse = ProfileApiResponded
                    , username = username
                    }
                , Api.Article.getFirst20ArticleBy
                    { onResponse = ArticleByAuthorApiResponded
                    , author = Just username
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
              , isFavoriteButtonClicked = False
              , token = Maybe.map (\u -> u.token) mayBeUser
              , username = username
              , isFollowedButtonClicked = False
              }
            , Effect.batch
                [ Api.Profile.getProfile
                    { onResponse = ProfileApiResponded
                    , username = username
                    }
                , Api.Article.getFirst20ArticleBy
                    { onResponse = ArticleByAuthorApiResponded
                    , author = Just username
                    , favorited = Nothing
                    , token = Nothing
                    }
                ]
            )



-- UPDATE


type Msg
    = ProfileApiResponded (Result Http.Error Profile)
    | ArticleByAuthorApiResponded (Result Http.Error (List Article))
    | UserClickedMyArticle
    | UserClickedFavoritedArticle
    | UserClickedOnFavoriteArticle String
    | UserClickedOnUnFavoriteArticle String
    | ArticleFavoriteApiResponded (Result Http.Error Article)
    | ArticleUnFavoriteApiResponded (Result Http.Error Article)
    | UserClickedFollow
    | UserClickedUnFollow


update : String -> Msg -> Model -> ( Model, Effect Msg )
update username msg model =
    case msg of
        ProfileApiResponded (Ok profile) ->
            ( { model | profileData = Api.Success profile, isFollowedButtonClicked = False }
            , Effect.none
            )

        ProfileApiResponded (Err httpError) ->
            ( { model | profileData = Api.Failure httpError, isFollowedButtonClicked = False }
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

        UserClickedFavoritedArticle ->
            ( { model | selectedFeedTab = FavoriteArticle }
            , Api.Article.getFirst20ArticleBy
                { onResponse = ArticleByAuthorApiResponded
                , author = Nothing
                , favorited = Just username
                , token = Nothing
                }
            )

        UserClickedMyArticle ->
            ( { model | selectedFeedTab = MyArticle }
            , Api.Article.getFirst20ArticleBy
                { onResponse = ArticleByAuthorApiResponded
                , author = Just username
                , favorited = Nothing
                , token = Nothing
                }
            )

        UserClickedOnFavoriteArticle slug ->
            ( { model | isFavoriteButtonClicked = True }
            , Api.Article.favoriteArticleCommets
                { onResponse = ArticleFavoriteApiResponded
                , token = model.token
                , slug = slug
                }
            )

        UserClickedOnUnFavoriteArticle slug ->
            ( { model | isFavoriteButtonClicked = True }
            , Api.Article.unfavoriteArticleCommets
                { onResponse = ArticleUnFavoriteApiResponded
                , token = model.token
                , slug = slug
                }
            )

        ArticleFavoriteApiResponded (Ok _) ->
            ( { model | isFavoriteButtonClicked = False }
            , favoriteApiCallBack model.username model.token model.selectedFeedTab
            )

        ArticleFavoriteApiResponded (Err _) ->
            ( { model | isFavoriteButtonClicked = False }
            , Effect.none
            )

        ArticleUnFavoriteApiResponded (Ok _) ->
            ( { model | isFavoriteButtonClicked = False }
            , favoriteApiCallBack model.username model.token model.selectedFeedTab
            )

        ArticleUnFavoriteApiResponded (Err _) ->
            ( { model | isFavoriteButtonClicked = False }
            , Effect.none
            )

        UserClickedFollow ->
            ( { model | isFollowedButtonClicked = True }
            , Api.Profile.followUser
                { onResponse = ProfileApiResponded
                , token = model.token
                , username = username
                }
            )

        UserClickedUnFollow ->
            ( { model | isFollowedButtonClicked = True }
            , Api.Profile.unFollowUser
                { onResponse = ProfileApiResponded
                , token = model.token
                , username = username
                }
            )


favoriteApiCallBack : String -> Maybe String -> SelectedArticle -> Effect Msg
favoriteApiCallBack username token selector =
    case selector of
        MyArticle ->
            Api.Article.getFirst20ArticleBy
                { onResponse = ArticleByAuthorApiResponded
                , author = Just username
                , favorited = Nothing
                , token = token
                }

        FavoriteArticle ->
            Api.Article.getFirst20ArticleBy
                { onResponse = ArticleByAuthorApiResponded
                , author = Nothing
                , favorited = Just username
                , token = token
                }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : String -> Model -> View Msg
view username model =
    { title = username
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
                                    [ Attr.classList [ ( "nav-link", True ), ( "active", model.selectedFeedTab == MyArticle ) ]
                                    , Attr.href ""
                                    , onClick UserClickedMyArticle
                                    ]
                                    [ text "My Articles" ]
                                ]
                            , li
                                [ Attr.class "nav-item"
                                ]
                                [ a
                                    [ Attr.classList [ ( "nav-link", True ), ( "active", model.selectedFeedTab == FavoriteArticle ) ]
                                    , Attr.href ""
                                    , onClick UserClickedFavoritedArticle
                                    ]
                                    [ text "Favorited Articles" ]
                                ]
                            ]
                        ]
                        :: articleCardsView model
                    )
                ]
            ]
        ]


articleCardsView : Model -> List (Html Msg)
articleCardsView model =
    case model.articleData of
        Api.Loading ->
            [ div []
                [ Html.text "Loading..."
                ]
            ]

        Api.Failure httpError ->
            [ div []
                [ Html.text (Api.Article.toUserFriendlyMessage httpError)
                ]
            ]

        Api.Success articleList ->
            List.map (articleCardView model.isFavoriteButtonClicked) articleList


articleCardView : Bool -> Article -> Html Msg
articleCardView isFavoriteButtonClicked article =
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
                [ Attr.classList
                    [ ( "btn btn-sm pull-xs-right", True )
                    , ( "btn-outline-primary", not article.favorited )
                    , ( "btn-primary", article.favorited )
                    , ( "disabled", isFavoriteButtonClicked )
                    ]
                , Html.Events.onClick
                    (if article.favorited then
                        UserClickedOnUnFavoriteArticle article.slug

                     else
                        UserClickedOnFavoriteArticle article.slug
                    )
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
                , followButon model profile username
                ]


followButon : Model -> Profile -> String -> Html Msg
followButon model profile username =
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
            [ Attr.classList [ ( "btn btn-sm btn-outline-secondary action-btn", True ), ( "disabled", model.isFollowedButtonClicked ) ]
            , Html.Events.onClick
                (if profile.following then
                    UserClickedUnFollow

                 else
                    UserClickedFollow
                )
            ]
            [ i
                [ Attr.class "ion-plus-round"
                ]
                []
            , if profile.following then
                text (" UnFollow " ++ profile.username)

              else
                text (" Follow " ++ profile.username)
            ]



--- Api


mydateFormat : String -> String
mydateFormat d =
    let
        date =
            toTime d
    in
    case date of
        Ok pdate ->
            Date.format "MMMM d, y" (Date.fromPosix utc pdate)

        Err _ ->
            "err"
