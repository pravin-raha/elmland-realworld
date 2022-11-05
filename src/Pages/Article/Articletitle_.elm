module Pages.Article.Articletitle_ exposing (Model, Msg, page)

import Api exposing (Data(..))
import Api.Article exposing (Article, Comment)
import Api.Profile exposing (Profile)
import Auth
import Date
import Dict
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events
import Http
import Iso8601 exposing (toTime)
import Json.Decode
import Json.Encode
import Layout exposing (Layout)
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import Time exposing (utc)
import View exposing (View)


layout : Layout
layout =
    Layout.HeaderAndFooter


page : Auth.User -> Shared.Model -> Route { articletitle : String } -> Page Model Msg
page user _ route =
    Page.new
        { init = init user route.params.articletitle
        , update = update route
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { articleData : Api.Data Article
    , commentsData : Api.Data (List Comment)
    , slug : String
    , loggedInuser : Auth.User
    , errors : List FormError
    , isSubmittingForm : Bool
    , commentBody : String
    , commentId : Maybe Int
    , isFavoriteButtonClicked : Bool
    , isFollowedButtonClicked : Bool
    }


init : Auth.User -> String -> () -> ( Model, Effect Msg )
init maybeUser slug () =
    let
        token =
            Maybe.map (\u -> u.token) maybeUser
    in
    ( { articleData = Api.Loading
      , commentsData = Api.Loading
      , slug = slug
      , loggedInuser = maybeUser
      , errors = []
      , isSubmittingForm = False
      , commentBody = ""
      , commentId = Nothing
      , isFavoriteButtonClicked = False
      , isFollowedButtonClicked = False
      }
    , Effect.batch
        [ Api.Article.getArticle
            { onResponse = ArticleApiResponded
            , token = token
            , slug = slug
            }
        , Api.Article.getArticleCommets
            { onResponse = ArticleCommentsApiResponded
            , token = token
            , slug = slug
            }
        ]
    )



-- UPDATE


type Msg
    = ArticleApiResponded (Result Http.Error Article)
    | ArticleCommentsApiResponded (Result Http.Error (List Comment))
    | UserSubmittedForm
    | UserUpdatedInput Field String
    | ArticleCommentCreateApiResponded (Result (List FormError) Comment)
    | ArticleCommentDeletedApiResponded (Result Http.Error String)
    | UserClickedOnDeleteComment Int
    | UserClickedOnFavoriteArticle String
    | UserClickedOnUnFavoriteArticle String
    | ArticleFavoriteApiResponded (Result Http.Error Article)
    | ArticleUnFavoriteApiResponded (Result Http.Error Article)
    | UserClickedFollow String
    | UserClickedUnFollow String
    | ProfileApiResponded (Result Http.Error Profile)
    | UserClickedOnDeleteArticle String
    | UserClickedOnEditArticle String
    | DeleteArticleAPiResponded (Result Http.Error String)


update : Route { articletitle : String } -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        ArticleApiResponded (Ok article) ->
            ( { model | articleData = Api.Success article, isFollowedButtonClicked = False }
            , Effect.none
            )

        ArticleApiResponded (Err httpError) ->
            ( { model | articleData = Api.Failure httpError, isFollowedButtonClicked = False }
            , Effect.none
            )

        ArticleCommentsApiResponded (Ok comments) ->
            ( { model | commentsData = Api.Success comments }
            , Effect.none
            )

        ArticleCommentsApiResponded (Err httpError) ->
            ( { model | articleData = Api.Failure httpError }
            , Effect.none
            )

        UserSubmittedForm ->
            ( { model
                | isSubmittingForm = True
                , errors = []
                , commentBody = ""
              }
            , Effect.fromCmd
                (callCreateArticleCommentApi
                    { commentBody = model.commentBody
                    , slug = model.slug
                    , token = Maybe.withDefault "" (Maybe.map (\u -> u.token) model.loggedInuser)
                    }
                )
            )

        UserUpdatedInput Comment value ->
            ( { model
                | commentBody = value
                , errors = clearErrorsForm Comment model.errors
              }
            , Effect.none
            )

        ArticleCommentCreateApiResponded (Err formErrors) ->
            ( { model | errors = formErrors, isSubmittingForm = False }
            , Effect.none
            )

        ArticleCommentCreateApiResponded (Ok _) ->
            ( model
            , Api.Article.getArticleCommets
                { onResponse = ArticleCommentsApiResponded
                , token = Maybe.map (\u -> u.token) model.loggedInuser
                , slug = model.slug
                }
            )

        ArticleCommentDeletedApiResponded (Err _) ->
            ( model
            , Effect.none
            )

        ArticleCommentDeletedApiResponded (Ok _) ->
            ( model
            , Api.Article.getArticleCommets
                { onResponse = ArticleCommentsApiResponded
                , token = Maybe.map (\u -> u.token) model.loggedInuser
                , slug = model.slug
                }
            )

        UserClickedOnDeleteComment id ->
            ( model
            , Effect.fromCmd
                (callDeleteArticleCommentApi
                    { id = id
                    , slug = model.slug
                    , token = Maybe.withDefault "" (Maybe.map (\u -> u.token) model.loggedInuser)
                    }
                )
            )

        UserClickedOnFavoriteArticle slug ->
            ( { model | isFavoriteButtonClicked = True }
            , Api.Article.favoriteArticleCommets
                { onResponse = ArticleFavoriteApiResponded
                , token = Maybe.map (\u -> u.token) model.loggedInuser
                , slug = slug
                }
            )

        UserClickedOnUnFavoriteArticle slug ->
            ( { model | isFavoriteButtonClicked = True }
            , Api.Article.unfavoriteArticleCommets
                { onResponse = ArticleUnFavoriteApiResponded
                , token = Maybe.map (\u -> u.token) model.loggedInuser
                , slug = slug
                }
            )

        ArticleFavoriteApiResponded (Ok article) ->
            ( { model | isFavoriteButtonClicked = False, articleData = Api.Success article }
            , Effect.none
            )

        ArticleFavoriteApiResponded (Err _) ->
            ( { model | isFavoriteButtonClicked = False }
            , Effect.none
            )

        ArticleUnFavoriteApiResponded (Ok article) ->
            ( { model | isFavoriteButtonClicked = False, articleData = Api.Success article }
            , Effect.none
            )

        ArticleUnFavoriteApiResponded (Err _) ->
            ( { model | isFavoriteButtonClicked = False }
            , Effect.none
            )

        UserClickedFollow username ->
            ( { model | isFollowedButtonClicked = True }
            , Api.Profile.followUser
                { onResponse = ProfileApiResponded
                , token = Maybe.map (\u -> u.token) model.loggedInuser
                , username = username
                }
            )

        UserClickedUnFollow username ->
            ( { model | isFollowedButtonClicked = True }
            , Api.Profile.unFollowUser
                { onResponse = ProfileApiResponded
                , token = Maybe.map (\u -> u.token) model.loggedInuser
                , username = username
                }
            )

        ProfileApiResponded (Ok _) ->
            ( { model | isFollowedButtonClicked = True }
            , Api.Article.getArticle
                { onResponse = ArticleApiResponded
                , token = Maybe.map (\u -> u.token) model.loggedInuser
                , slug = model.slug
                }
            )

        ProfileApiResponded (Err _) ->
            ( { model | isFollowedButtonClicked = True }
            , Effect.none
            )

        UserClickedOnDeleteArticle slug ->
            ( model
            , Api.Article.deleteArticleApi
                { onResponse = DeleteArticleAPiResponded
                , slug = slug
                , token = Maybe.withDefault "" (Maybe.map (\u -> u.token) model.loggedInuser)
                }
            )

        UserClickedOnEditArticle slug ->
            ( model
            , Effect.none
            )

        DeleteArticleAPiResponded (Err _) ->
            ( model
            , Effect.none
            )

        DeleteArticleAPiResponded (Ok _) ->
            ( model
            , Effect.replaceRoute
                { path = Route.Path.Home_
                , query = Dict.fromList [ ( "from", route.url.path ) ]
                , hash = Nothing
                }
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = model.slug
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
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
                [ titleView model article
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
                            ([ a
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
                             ]
                                ++ followOrEditButton model article
                            )
                        ]
                    , div
                        [ Attr.class "row"
                        ]
                        [ div
                            [ Attr.class "col-xs-12 col-md-8 offset-md-2"
                            ]
                            (postComment model :: commentListView model)
                        ]
                    ]
                ]

        Failure httpError ->
            div []
                [ Html.text (Api.Article.toUserFriendlyMessage httpError)
                ]


titleView : Model -> Article -> Html Msg
titleView model article =
    div [ Attr.class "banner" ]
        [ div
            [ Attr.class "container"
            ]
            [ h1 []
                [ text article.title ]
            , div
                [ Attr.class "article-meta"
                ]
                ([ a
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
                 ]
                    ++ followOrEditButton model article
                )
            ]
        ]


followOrEditButton : Model -> Article -> List (Html Msg)
followOrEditButton model article =
    if Maybe.withDefault "" (Maybe.map (\u -> u.username) model.loggedInuser) == article.author.username then
        editDeleteButtonView model

    else
        followButtonView model article


followButtonView : Model -> Article -> List (Html Msg)
followButtonView model article =
    [ button
        [ Attr.classList [ ( "btn btn-sm btn-outline-secondary", True ), ( "disabled", model.isFollowedButtonClicked ) ]
        , Html.Events.onClick
            (if article.author.following then
                UserClickedUnFollow article.author.username

             else
                UserClickedFollow article.author.username
            )
        ]
        [ i
            [ Attr.class "ion-plus-round"
            ]
            []
        , if article.author.following then
            text (" UnFollow " ++ article.author.username)

          else
            text (" Follow " ++ article.author.username)
        ]
    , button
        [ Attr.classList
            [ ( "btn btn-sm", True )
            , ( "btn-outline-primary", not article.favorited )
            , ( "btn-primary", article.favorited )
            , ( "disabled", model.isFavoriteButtonClicked )
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
        , text " Favorite Post"
        , span
            [ Attr.class "counter"
            ]
            [ text ("(" ++ String.fromInt article.favoritesCount ++ ")") ]
        ]
    ]


editDeleteButtonView : Model -> List (Html Msg)
editDeleteButtonView model =
    [ button
        [ Attr.classList [ ( "btn btn-sm btn-outline-secondary", True ), ( "disabled", model.isFollowedButtonClicked ) ]
        , Html.Events.onClick (UserClickedOnEditArticle model.slug)
        ]
        [ i
            [ Attr.class "ion-edit"
            ]
            []
        , text "Edit Article"
        ]
    , button
        [ Attr.classList
            [ ( "btn btn-sm btn-outline-danger", True )
            , ( "disabled", model.isFavoriteButtonClicked )
            ]
        , Html.Events.onClick (UserClickedOnDeleteArticle model.slug)
        ]
        [ i
            [ Attr.class "ion-trash-a"
            ]
            []
        , text "Delete Article"
        , span [] [ text "" ]
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

        Err _ ->
            "err"


commentListView : Model -> List (Html Msg)
commentListView model =
    case model.commentsData of
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

        Api.Success commnets ->
            List.map (commentCardView (Maybe.withDefault "" (Maybe.map (\u -> u.username) model.loggedInuser))) commnets


commentCardView : String -> Comment -> Html Msg
commentCardView username comment =
    let
        editDeleteOption =
            if username == comment.author.username then
                [ span
                    [ Attr.class "mod-options"
                    ]
                    [ i
                        [ Attr.class "ion-trash-a"
                        , Html.Events.onClick (UserClickedOnDeleteComment comment.id)
                        ]
                        []
                    ]
                ]

            else
                []
    in
    div
        [ Attr.class "card"
        ]
        [ div
            [ Attr.class "card-block"
            ]
            [ p
                [ Attr.class "card-text"
                ]
                [ text comment.body ]
            ]
        , div
            [ Attr.class "card-footer"
            ]
            ([ a
                [ Attr.href ("/profile/" ++ comment.author.username)
                , Attr.class "comment-author"
                ]
                [ img
                    [ Attr.src comment.author.image
                    , Attr.class "comment-author-img"
                    ]
                    []
                ]
             , a
                [ Attr.href ("/profile/" ++ comment.author.username)
                , Attr.class "comment-author"
                ]
                [ text (" " ++ comment.author.username) ]
             , span
                [ Attr.class "date-posted"
                ]
                [ text (mydateFormat comment.updatedAt) ]
             ]
                ++ editDeleteOption
            )
        ]


postComment : Model -> Html Msg
postComment model =
    case model.loggedInuser of
        Nothing ->
            p
                [ Attr.attribute "show-authed" "false"
                , Attr.style "display" "inherit"
                ]
                [ a
                    [ Attr.attribute "ui-sref" "app.login"
                    , Attr.href "/login"
                    ]
                    [ text "Sign in" ]
                , text " or "
                , a
                    [ Attr.attribute "ui-sref" "app.register"
                    , Attr.href "/register"
                    ]
                    [ text "sign up " ]
                , text "to add comments on this article."
                ]

        Just user ->
            form
                [ Attr.class "card comment-form"
                , Html.Events.onSubmit UserSubmittedForm
                ]
                [ div
                    [ Attr.class "card-block"
                    ]
                    [ textarea
                        [ Attr.class "form-control"
                        , Attr.placeholder "Write a comment..."
                        , Attr.rows 3
                        , Html.Events.onInput (UserUpdatedInput Comment)
                        , Attr.value model.commentBody
                        ]
                        []
                    ]
                , div
                    [ Attr.class "card-footer"
                    ]
                    [ img
                        [ Attr.src user.image
                        , Attr.class "comment-author-img"
                        ]
                        []
                    , button
                        [ Attr.class "btn btn-sm btn-primary"
                        ]
                        [ text "Post Comment" ]
                    ]
                ]



-- Form


type Field
    = Comment


type alias FormError =
    { field : Maybe Field
    , message : String
    }


clearErrorsForm : Field -> List FormError -> List FormError
clearErrorsForm field errors =
    errors
        |> List.filter (\error -> error.field /= Just field)


callCreateArticleCommentApi :
    { commentBody : String
    , slug : String
    , token : String
    }
    -> Cmd Msg
callCreateArticleCommentApi payload =
    let
        json : Json.Encode.Value
        json =
            Json.Encode.object
                [ ( "comment"
                  , Json.Encode.object
                        [ ( "body", Json.Encode.string payload.commentBody )
                        ]
                  )
                ]
    in
    Http.request
        { method = "post"
        , url = "https://api.realworld.io/api/articles/" ++ payload.slug ++ "/comments"
        , body = Http.jsonBody json
        , expect = expectApiResponse ArticleCommentCreateApiResponded Api.Article.singleArticleCommentDecoder
        , headers = [ Http.header "Authorization" ("Bearer " ++ payload.token) ]
        , timeout = Nothing
        , tracker = Nothing
        }


callDeleteArticleCommentApi :
    { id : Int
    , slug : String
    , token : String
    }
    -> Cmd Msg
callDeleteArticleCommentApi payload =
    Http.request
        { method = "DELETE"
        , url = "https://api.realworld.io/api/articles/" ++ payload.slug ++ "/comments/" ++ String.fromInt payload.id
        , body = Http.emptyBody
        , expect = Http.expectString ArticleCommentDeletedApiResponded
        , headers = [ Http.header "Authorization" ("Bearer " ++ payload.token) ]
        , timeout = Nothing
        , tracker = Nothing
        }


expectApiResponse :
    (Result (List FormError) value -> msg)
    -> Json.Decode.Decoder value
    -> Http.Expect msg
expectApiResponse toMsg decoder =
    Http.expectStringResponse toMsg (toFormApiResult decoder)


toFormApiResult : Json.Decode.Decoder value -> Http.Response String -> Result (List FormError) value
toFormApiResult decoder response =
    case response of
        Http.BadUrl_ _ ->
            Err [ { field = Nothing, message = "Unexpected URL format" } ]

        Http.Timeout_ ->
            Err [ { field = Nothing, message = "Server did not respond" } ]

        Http.NetworkError_ ->
            Err [ { field = Nothing, message = "Could not connect to server" } ]

        Http.BadStatus_ { statusCode } rawJson ->
            case Json.Decode.decodeString formErrorsDecoder rawJson of
                Ok errors ->
                    Err errors

                Err _ ->
                    Err [ { field = Nothing, message = "Received status code " ++ String.fromInt statusCode } ]

        Http.GoodStatus_ _ rawJson ->
            case Json.Decode.decodeString decoder rawJson of
                Ok value ->
                    Ok value

                Err _ ->
                    Err [ { field = Nothing, message = "Received unexpected API response" } ]


formErrorsDecoder : Json.Decode.Decoder (List FormError)
formErrorsDecoder =
    let
        formErrorDecoder : Json.Decode.Decoder FormError
        formErrorDecoder =
            Json.Decode.map2 FormError
                (Json.Decode.field "field" Json.Decode.string
                    |> Json.Decode.map fromStringToMaybeField
                )
                (Json.Decode.field "message" Json.Decode.string)

        fromStringToMaybeField : String -> Maybe Field
        fromStringToMaybeField field =
            case field of
                "body" ->
                    Just Comment

                _ ->
                    Nothing
    in
    Json.Decode.field "errors" (Json.Decode.list formErrorDecoder)
