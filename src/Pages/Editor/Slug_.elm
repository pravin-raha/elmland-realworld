module Pages.Editor.Slug_ exposing (Model, Msg, page)

import Api.Article exposing (Article)
import Auth
import Dict
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events
import Http
import Json.Decode exposing (list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)


layout : Auth.User -> Model -> Layouts.Layout
layout user model =
    Layouts.HeaderAndFooter
        { headerAndFooter =
            { title = "Edit Article"
            , user = user
            }
        }


page : Auth.User -> Shared.Model -> Route { slug : String } -> Page Model Msg
page user _ route =
    Page.new
        { init = init user route
        , update = update user route
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (layout user)



-- INIT


type alias Form =
    { title : String
    , body : String
    , description : String
    , tagList : List String
    }


type alias Model =
    { form : Maybe Form
    , errors : List FormError
    , isSubmittingForm : Bool
    , slug : String
    }


init : Auth.User -> Route { slug : String } -> () -> ( Model, Effect Msg )
init maybeUser route () =
    case maybeUser of
        Just user ->
            ( { form = Nothing
              , errors = []
              , isSubmittingForm = False
              , slug = route.params.slug
              }
            , Api.Article.getArticle
                { onResponse = ArticleApiResponded
                , token = Just user.token
                , slug = route.params.slug
                }
            )

        Nothing ->
            ( { form = Nothing
              , errors = []
              , isSubmittingForm = False
              , slug = route.params.slug
              }
            , Effect.replaceRoute
                { path = Route.Path.Login
                , query = Dict.fromList [ ( "from", route.url.path ) ]
                , hash = Nothing
                }
            )



-- UPDATE


type Msg
    = UserUpdatedInput Field String
    | UserSubmittedForm
    | ArticleUpdatedApiResponded (Result (List FormError) CreateArticlePayload)
    | ArticleApiResponded (Result Http.Error Article)


update : Auth.User -> Route { slug : String } -> Msg -> Model -> ( Model, Effect Msg )
update mayBeUser route msg model =
    case mayBeUser of
        Just user ->
            case msg of
                UserUpdatedInput Title value ->
                    ( { model
                        | form = Maybe.map (\f -> { f | title = value }) model.form
                        , errors = clearErrorsForm Title model.errors
                      }
                    , Effect.none
                    )

                UserUpdatedInput Description value ->
                    ( { model
                        | form = Maybe.map (\f -> { f | description = value }) model.form
                        , errors = clearErrorsForm Description model.errors
                      }
                    , Effect.none
                    )

                UserUpdatedInput Body value ->
                    ( { model
                        | form = Maybe.map (\f -> { f | body = value }) model.form
                        , errors = clearErrorsForm Body model.errors
                      }
                    , Effect.none
                    )

                UserUpdatedInput TagList value ->
                    ( { model
                        | form = Maybe.map (\f -> { f | tagList = [ value ] }) model.form
                        , errors = clearErrorsForm TagList model.errors
                      }
                    , Effect.none
                    )

                UserSubmittedForm ->
                    ( { model
                        | isSubmittingForm = True
                        , errors = []
                      }
                    , Effect.fromCmd
                        (callUpdateArticleApi
                            { form =
                                Maybe.withDefault
                                    { title = ""
                                    , body = ""
                                    , description = ""
                                    , tagList = []
                                    }
                                    model.form
                            , token = user.token
                            , slug = model.slug
                            }
                        )
                    )

                ArticleUpdatedApiResponded (Err formErrors) ->
                    ( { model | errors = formErrors, isSubmittingForm = False }
                    , Effect.none
                    )

                ArticleUpdatedApiResponded (Ok _) ->
                    ( model
                    , Effect.replaceRoute
                        { path = Route.Path.Home_
                        , query = Dict.fromList [ ( "from", route.url.path ) ]
                        , hash = Nothing
                        }
                    )

                ArticleApiResponded (Ok article) ->
                    ( { model
                        | form =
                            Just
                                { title = article.title
                                , body = article.body
                                , description = article.description
                                , tagList = article.tagList
                                }
                      }
                    , Effect.none
                    )

                ArticleApiResponded (Err _) ->
                    ( model
                    , Effect.none
                    )

        Nothing ->
            ( model
            , Effect.replaceRoute
                { path = Route.Path.Login
                , query = Dict.fromList [ ( "from", route.url.path ) ]
                , hash = Nothing
                }
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Edit Article"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    case model.form of
        Nothing ->
            div []
                [ Html.text "Loading..."
                ]

        Just article ->
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
                            [ form [ Html.Events.onSubmit UserSubmittedForm ]
                                [ fieldset []
                                    [ fieldset
                                        [ Attr.class "form-group"
                                        ]
                                        [ input
                                            [ Attr.type_ "text"
                                            , Attr.class "form-control form-control-lg"
                                            , Attr.placeholder "Article Title"
                                            , Attr.value article.title
                                            , Html.Events.onInput (UserUpdatedInput Title)
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
                                            , Attr.value article.description
                                            , Html.Events.onInput (UserUpdatedInput Description)
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
                                            , Attr.value article.body
                                            , Html.Events.onInput (UserUpdatedInput Body)
                                            ]
                                            []
                                        ]
                                    , fieldset
                                        -- TODO: Fix multiple tags
                                        [ Attr.class "form-group"
                                        ]
                                        [ input
                                            [ Attr.type_ "text"
                                            , Attr.class "form-control"
                                            , Attr.placeholder "Enter tags"
                                            , Attr.value (Maybe.withDefault "" (List.head article.tagList))
                                            , Html.Events.onInput (UserUpdatedInput TagList)
                                            ]
                                            []
                                        , div
                                            [ Attr.class "tag-list"
                                            ]
                                            []
                                        ]
                                    , button
                                        [ Attr.class "btn btn-lg pull-xs-right btn-primary" ]
                                        [ text "Publish Article" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]



-- Form


type alias CreateArticlePayload =
    { title : String
    , body : String
    , description : String
    , tagList : List String
    }


type Field
    = Title
    | Description
    | Body
    | TagList


type alias FormError =
    { field : Maybe Field
    , message : String
    }


callUpdateArticleApi :
    { form : Form
    , token : String
    , slug : String
    }
    -> Cmd Msg
callUpdateArticleApi payload =
    let
        json : Json.Encode.Value
        json =
            Json.Encode.object
                [ ( "article"
                  , Json.Encode.object
                        [ ( "title", Json.Encode.string payload.form.title )
                        , ( "description", Json.Encode.string payload.form.description )
                        , ( "body", Json.Encode.string payload.form.body )
                        , ( "tagList", Json.Encode.list Json.Encode.string payload.form.tagList )
                        ]
                  )
                ]
    in
    Http.request
        { method = "PUT"
        , url = "https://api.realworld.io/api/articles/" ++ payload.slug
        , body = Http.jsonBody json
        , expect = expectApiResponse ArticleUpdatedApiResponded articleDecoder
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
                "title" ->
                    Just Title

                "description" ->
                    Just Description

                "body" ->
                    Just Body

                "tagList" ->
                    Just TagList

                _ ->
                    Nothing
    in
    Json.Decode.field "errors" (Json.Decode.list formErrorDecoder)


clearErrorsForm : Field -> List FormError -> List FormError
clearErrorsForm field errors =
    errors
        |> List.filter (\error -> error.field /= Just field)


articleDecoder : Json.Decode.Decoder CreateArticlePayload
articleDecoder =
    Json.Decode.field "article"
        (Json.Decode.succeed CreateArticlePayload
            |> required "title" string
            |> required "body" string
            |> required "description" string
            |> required "tagList" (list string)
        )
