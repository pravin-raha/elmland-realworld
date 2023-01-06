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
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared 
import Shared.Model exposing (SignInStatus(..))
import Shared.Msg
import Time exposing (Month(..), utc)
import View exposing (View)


layout : Auth.User -> Model -> Layouts.Layout
layout user model =
    Layouts.HeaderAndFooter
        { headerAndFooter =
            { title = "Home -Conduit"
            , user = user
            }
        }


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user smodel _ =
    Page.new
        { init = init user
        , update = update user smodel
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (layout user)



-- INIT


type SelectedFeed
    = GlobalFeed
    | YourFeed
    | TagFeed


type alias Model =
    { articleData : Api.Data (List Article)
    , popularTagData : Api.Data (List String)
    , selectedFeedTab : SelectedFeed
    , userSignIn : Bool
    , token : Maybe String
    , isFavoriteButtonClicked : Bool
    , clickedTag : String
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

        token =
            Maybe.map (\u -> u.token) maybeUser
    in
    ( { articleData = Api.Loading
      , popularTagData = Api.Loading
      , selectedFeedTab = GlobalFeed
      , userSignIn = userSignIn
      , token = token
      , isFavoriteButtonClicked = False
      , clickedTag = ""
      }
    , Effect.batch
        [ Api.Article.getFirst20ArticleBy
            { onResponse = ArticleApiResponded
            , author = Nothing
            , favorited = Nothing
            , tag = Nothing
            , token = token
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
    | UserClickedTagFeeds String
    | UserClickedGLobalArticle
    | UserClickedOnFavoriteArticle String
    | UserClickedOnUnFavoriteArticle String
    | ArticleFavoriteApiResponded (Result Http.Error Article)
    | ArticleUnFavoriteApiResponded (Result Http.Error Article)


update : Auth.User -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update maybeUser _ msg model =
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
                , tag = Nothing
                , token = model.token
                }
            )

        UserClickedTagFeeds tag ->
            ( { model | selectedFeedTab = TagFeed, clickedTag = tag }
            , Api.Article.getFirst20ArticleBy
                { onResponse = ArticleApiResponded
                , author = Nothing
                , favorited = Nothing
                , tag = Just tag
                , token = model.token
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
            , favoriteApiCallBack model.token model.selectedFeedTab
            )

        ArticleFavoriteApiResponded (Err _) ->
            ( { model | isFavoriteButtonClicked = False }
            , Effect.none
            )

        ArticleUnFavoriteApiResponded (Ok _) ->
            ( { model | isFavoriteButtonClicked = False }
            , favoriteApiCallBack model.token model.selectedFeedTab
              -- Change to in app state change
            )

        ArticleUnFavoriteApiResponded (Err _) ->
            ( { model | isFavoriteButtonClicked = False }
            , Effect.none
            )


favoriteApiCallBack : Maybe String -> SelectedFeed -> Effect Msg
favoriteApiCallBack token selector =
    case selector of
        GlobalFeed ->
            Api.Article.getFirst20ArticleBy
                { onResponse = ArticleApiResponded
                , author = Nothing
                , favorited = Nothing
                , tag = Nothing
                , token = token
                }

        YourFeed ->
            Api.Article.getFirst20Feeds
                { onResponse = ArticleApiResponded
                , token = Maybe.withDefault "" token
                }

        TagFeed ->
            Api.Article.getFirst20ArticleBy
                { onResponse = ArticleApiResponded
                , author = Nothing
                , favorited = Nothing
                , tag = Just "tag"
                , token = token
                }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Home -Conduit"
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
                (feedView model :: articleListView model)
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
                        , Attr.href ""
                        , onClick UserClickedFeeds
                        ]
                        [ text "Your Feed" ]
                    ]
                ]

            else
                []

        tagView =
            if model.selectedFeedTab == TagFeed then
                [ li
                    [ Attr.class "nav-item"
                    ]
                    [ a
                        [ Attr.classList [ ( "nav-link", True ), ( "active", model.selectedFeedTab == TagFeed ) ]
                        , Attr.href ""
                        , onClick (UserClickedTagFeeds model.clickedTag)
                        ]
                        [ text ("# " ++ model.clickedTag) ]
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
                    , Attr.href ""
                    , onClick UserClickedGLobalArticle
                    ]
                    [ text "Global Feed" ]
                ]
                :: yourFeedLiView
                ++ tagView
            )
        ]


popularTagView : Model -> Html Msg
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


popularTagListView : Model -> Html Msg
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


popularTagRowView : String -> Html Msg
popularTagRowView tag =
    a
        [ Attr.href ""
        , Html.Events.onClick (UserClickedTagFeeds tag)
        , Attr.class "tag-pill tag-default"
        ]
        [ text tag ]


articleListView : Model -> List (Html Msg)
articleListView model =
    case model.articleData of
        Api.Loading ->
            [ div []
                [ Html.text "Loading..."
                ]
            ]

        Api.Success articleList ->
            List.map (articleCardView model.isFavoriteButtonClicked) articleList

        Api.Failure httpError ->
            [ div []
                [ Html.text (Api.Article.toUserFriendlyMessage httpError)
                ]
            ]


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

        Err _ ->
            "err"
