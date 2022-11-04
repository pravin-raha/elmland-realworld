module Pages.Home_ exposing (Model, Msg, page)

import Api
import Api.Article exposing (Article)
import Api.PopularTagsList
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
import Shared exposing (SignInStatus(..))
import Shared.Msg
import Time exposing (Month(..), utc)
import View exposing (View)


layout : Layout
layout =
    Layout.HeaderAndFooter


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user smodel _ =
    Page.new
        { init = init user
        , update = update user smodel
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type SelectedFeed
    = GlobalFeed
    | YourFeed


type alias Model =
    { articleData : Api.Data (List Article)
    , popularTagData : Api.Data (List String)
    , selectedFeedTab : SelectedFeed
    , userSignIn : Bool
    }


init : Auth.User -> () -> ( Model, Effect Msg )
init maybeUser () =
    let
        userSignIn =
            case maybeUser of
                Nothing ->
                    False

                Just _ ->
                    True
    in
    ( { articleData = Api.Loading
      , popularTagData = Api.Loading
      , selectedFeedTab = GlobalFeed
      , userSignIn = userSignIn
      }
    , Effect.batch
        [ Api.Article.getFirst20ArticleBy
            { onResponse = ArticleApiResponded
            , author = Nothing
            , favorited = Nothing
            , token = Nothing
            }
        , Api.PopularTagsList.getTags
            { onResponse = PopularTagsApiResponded
            }
        ]
    )



-- UPDATE


type Msg
    = ArticleApiResponded (Result Http.Error (List Article))
    | PopularTagsApiResponded (Result Http.Error (List String))
    | UserClickedSignOut
    | UserClickedFeeds
    | UserClickedGLobalArticle


update : Auth.User -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update maybeUser smodel msg model =
    case msg of
        ArticleApiResponded (Ok listOfArticle) ->
            ( { model | articleData = Api.Success listOfArticle }
            , Effect.none
            )

        ArticleApiResponded (Err httpError) ->
            ( { model | articleData = Api.Failure httpError }
            , Effect.none
            )

        PopularTagsApiResponded (Ok listOfPopularTag) ->
            ( { model | popularTagData = Api.Success listOfPopularTag }
            , Effect.none
            )

        PopularTagsApiResponded (Err httpError) ->
            ( { model | articleData = Api.Failure httpError }
            , Effect.none
            )

        UserClickedSignOut ->
            ( { model | userSignIn = False }
            , Effect.fromSharedMsg Shared.Msg.PageSignedOutUser
            )

        UserClickedFeeds ->
            case maybeUser of
                Nothing ->
                    ( { model | selectedFeedTab = YourFeed }
                    , Effect.none
                    )

                Just user ->
                    ( { model | selectedFeedTab = YourFeed }
                    , Api.Article.getFirst20Feeds
                        { onResponse = ArticleApiResponded
                        , token = user.token
                        }
                    )

        UserClickedGLobalArticle ->
            ( { model | selectedFeedTab = GlobalFeed }
            , Api.Article.getFirst20ArticleBy
                { onResponse = ArticleApiResponded
                , author = Nothing
                , favorited = Nothing
                , token = Nothing
                }
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Homer -Conduit"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
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
        , articleView model
        ]


articleView : Model -> Html Msg
articleView model =
    div
        [ Attr.class "container page"
        ]
        [ div
            [ Attr.class "row"
            ]
            [ div
                [ Attr.class "col-md-9"
                ]
                (articleListView model ++ [ feedView model ])
            , popularTagView model
            ]
        ]


feedView : Model -> Html Msg
feedView model =
    let
        yourFeedLiView =
            if model.userSignIn then
                [ li
                    [ Attr.class "nav-item"
                    ]
                    [ a
                        [ Attr.classList [ ( "nav-link", True ), ( "active", model.selectedFeedTab == YourFeed ) ]
                        , Attr.href "#"
                        , onClick UserClickedFeeds
                        ]
                        [ text "Your Feed" ]
                    ]
                ]

            else
                []
    in
    div
        [ Attr.class "feed-toggle"
        ]
        [ ul
            [ Attr.class "nav nav-pills outline-active"
            ]
            (li
                [ Attr.class "nav-item"
                ]
                [ a
                    [ Attr.classList [ ( "nav-link", True ), ( "active", model.selectedFeedTab == GlobalFeed ) ]
                    , Attr.href "#"
                    , onClick UserClickedGLobalArticle
                    ]
                    [ text "Global Feed" ]
                ]
                :: yourFeedLiView
            )
        ]


popularTagView : Model -> Html msg
popularTagView model =
    div
        [ Attr.class "col-md-3"
        ]
        [ div
            [ Attr.class "sidebar"
            ]
            [ p []
                [ text "Popular Tags" ]
            , popularTagListView model
            ]
        ]


popularTagListView : Model -> Html msg
popularTagListView model =
    case model.popularTagData of
        Api.Loading ->
            div []
                [ Html.text "Loading..."
                ]

        Api.Success popularTagList ->
            div [ Attr.class "tag-list" ]
                (List.map popularTagRowView popularTagList)

        Api.Failure httpError ->
            div []
                [ Html.text (Api.Article.toUserFriendlyMessage httpError)
                ]


popularTagRowView : String -> Html msg
popularTagRowView tag =
    a
        [ Attr.href ""
        , Attr.class "tag-pill tag-default"
        ]
        [ text tag ]


articleListView : Model -> List (Html msg)
articleListView model =
    case model.articleData of
        Api.Loading ->
            [ div []
                [ Html.text "Loading..."
                ]
            ]

        Api.Success articleList ->
            List.map articleRowView articleList

        Api.Failure httpError ->
            [ div []
                [ Html.text (Api.Article.toUserFriendlyMessage httpError)
                ]
            ]


articleRowView : Article -> Html msg
articleRowView article =
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
                    -- [ text article.updatedAt ]
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
            [ Attr.href ("/article/" ++ article.slug)
            , Attr.class "preview-link"
            ]
            [ h1 []
                [ text article.title ]
            , p []
                [ text article.description ]
            , span []
                [ text "Read more..." ]
            , ul [ Attr.class "tag-list" ]
                (List.map feedTagsView article.tagList)
            ]
        ]


feedTagsView : String -> Html msg
feedTagsView tags =
    li
        [ Attr.class "tag-default tag-pill tag-outline"
        ]
        [ text tags ]


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
