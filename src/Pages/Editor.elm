module Pages.Editor exposing (Model, Msg, page)

import Auth
import Dict
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (keyCode, onClick)
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
            { title = "HNew Article"
            , user = user
            }
        }


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init user route
        , update = update user route
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (layout user)



-- INIT


type alias Model =
    { title : String
    , body : String
    , description : String
    , tagList : List String
    , errors : List FormError
    , isSubmittingForm : Bool
    , tempTag : String
    }


init : Auth.User -> Route () -> () -> ( Model, Effect Msg )
init user route () =
    case user of
        Just _ ->
            ( { title = ""
              , body = ""
              , description = ""
              , tagList = []
              , errors = []
              , isSubmittingForm = False
              , tempTag = ""
              }
            , Effect.none
            )

        Nothing ->
            ( { title = ""
              , body = ""
              , description = ""
              , tagList = []
              , errors = []
              , isSubmittingForm = False
              , tempTag = ""
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
    | ArticleCreateApiResponded (Result (List FormError) CreateArticlePayload)
    | TagInputKeyDown Int
    | RemoveTagPill String


update : Auth.User -> Route () -> Msg -> Model -> ( Model, Effect Msg )
update mayBeUser route msg model =
    case mayBeUser of
        Just user ->
            case msg of
                UserUpdatedInput Title value ->
                    ( { model
                        | title = value
                        , errors = clearErrorsForm Title model.errors
                      }
                    , Effect.none
                    )

                UserUpdatedInput Description value ->
                    ( { model
                        | description = value
                        , errors = clearErrorsForm Description model.errors
                      }
                    , Effect.none
                    )

                UserUpdatedInput Body value ->
                    ( { model
                        | body = value
                        , errors = clearErrorsForm Body model.errors
                      }
                    , Effect.none
                    )

                UserUpdatedInput TagList value ->
                    ( { model
                        | tempTag = value
                        , errors = clearErrorsForm TagList model.errors
                      }
                    , Effect.none
                    )

                UserSubmittedForm ->
                    ( { model
                        | isSubmittingForm = True
                        , errors = []
                      }
                    , Effect.sendCmd
                        (callCreateArticleApi
                            { title = model.title
                            , body = model.body
                            , description = model.description
                            , tagList = model.tagList
                            , token = user.token
                            }
                        )
                    )

                ArticleCreateApiResponded (Err formErrors) ->
                    ( { model | errors = formErrors, isSubmittingForm = False }
                    , Effect.none
                    )

                ArticleCreateApiResponded (Ok _) ->
                    ( model
                    , Effect.replaceRoute
                        { path = Route.Path.Home_
                        , query = Dict.fromList [ ( "from", route.url.path ) ]
                        , hash = Nothing
                        }
                    )

                TagInputKeyDown key ->
                    if key == 13 then
                        ( { model | tagList = model.tempTag :: model.tagList, tempTag = "" }
                        , Effect.none
                        )

                    else
                        ( model, Effect.none )

                RemoveTagPill tag ->
                    ( { model | tagList = List.filter (\t -> t /= tag) model.tagList }
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
    { title = "New Article"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
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
                    [ div []
                        [ fieldset []
                            [ fieldset
                                [ Attr.class "form-group"
                                ]
                                [ input
                                    [ Attr.type_ "text"
                                    , Attr.class "form-control form-control-lg"
                                    , Attr.placeholder "Article Title"
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
                                    , Html.Events.onInput (UserUpdatedInput Body)
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
                                    , Attr.value model.tempTag
                                    , Html.Events.onInput (UserUpdatedInput TagList)
                                    , onKeyDown TagInputKeyDown
                                    ]
                                    []
                                , div
                                    [ Attr.class "tag-list"
                                    ]
                                    (tagPillView model.tagList)
                                ]
                            , button
                                [ Attr.class "btn btn-lg pull-xs-right btn-primary"
                                , onClick UserSubmittedForm
                                ]
                                [ text "Publish Article" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


tagPillView : List String -> List (Html Msg)
tagPillView tags =
    List.map
        (\t ->
            span
                [ Attr.class "tag-default tag-pill"
                ]
                [ i [ Attr.class "ion-close-round", Html.Events.onClick (RemoveTagPill t) ] []
                , text t
                ]
        )
        tags



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


callCreateArticleApi :
    { title : String
    , body : String
    , description : String
    , tagList : List String
    , token : String
    }
    -> Cmd Msg
callCreateArticleApi payload =
    let
        json : Json.Encode.Value
        json =
            Json.Encode.object
                [ ( "article"
                  , Json.Encode.object
                        [ ( "title", Json.Encode.string payload.title )
                        , ( "description", Json.Encode.string payload.description )
                        , ( "body", Json.Encode.string payload.body )
                        , ( "tagList", Json.Encode.list Json.Encode.string payload.tagList )
                        ]
                  )
                ]
    in
    Http.request
        { method = "POST"
        , url = "https://api.realworld.io/api/articles"
        , body = Http.jsonBody json
        , expect = expectApiResponse ArticleCreateApiResponded articleDecoder
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


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    Html.Events.on "keydown" (Json.Decode.map tagger keyCode)
