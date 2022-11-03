module Pages.Settings exposing (Model, Msg, page)

import Api.User
import Auth
import Dict
import Effect exposing (Effect)
import Html
import Html.Attributes as Attr
import Html.Events
import Http
import Json.Decode
import Json.Encode
import Layout exposing (Layout)
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Msg exposing (Msg(..))
import View exposing (View)


layout : Layout
layout =
    Layout.HeaderAndFooter


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init user route
        , update = update user
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { username : String
    , image : String
    , email : String
    , bio : Maybe String
    , password : String
    , errors : List FormError
    , isSubmittingForm : Bool
    }


init : Auth.User -> Route () -> () -> ( Model, Effect Msg )
init maybeUser route () =
    let
        model =
            { username = ""
            , image = ""
            , email = ""
            , bio = Nothing
            , password = ""
            , errors = []
            , isSubmittingForm = False
            }
    in
    case maybeUser of
        Just _ ->
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



-- UPDATE


type Msg
    = UserUpdateApiResponded (Result (List FormError) Api.User.User)
    | UserUpdatedInput Field String
    | UserSubmittedForm


update : Auth.User -> Msg -> Model -> ( Model, Effect Msg )
update mayBeUser msg model =
    case mayBeUser of
        Just user ->
            case msg of
                UserUpdatedInput Email value ->
                    ( { model
                        | email = value
                        , errors = clearErrorsFor Email model.errors
                      }
                    , Effect.none
                    )

                UserUpdatedInput UserName value ->
                    ( { model
                        | username = value
                        , errors = clearErrorsFor UserName model.errors
                      }
                    , Effect.none
                    )

                UserUpdatedInput Image value ->
                    ( { model
                        | image = value
                        , errors = clearErrorsFor Image model.errors
                      }
                    , Effect.none
                    )

                UserUpdatedInput Bio value ->
                    ( { model
                        | bio = Just value
                        , errors = clearErrorsFor Bio model.errors
                      }
                    , Effect.none
                    )

                UserUpdatedInput Password value ->
                    ( { model
                        | bio = Just value
                        , errors = clearErrorsFor Password model.errors
                      }
                    , Effect.none
                    )

                UserSubmittedForm ->
                    ( { model
                        | isSubmittingForm = True
                        , errors = []
                      }
                    , Effect.fromCmd
                        (callUserPutApi
                            { email = model.email
                            , username = model.username
                            , image = model.image
                            , bio = model.bio
                            , token = user.token
                            }
                        )
                    )

                UserUpdateApiResponded (Err formErrors) ->
                    ( { model | errors = formErrors, isSubmittingForm = False }
                    , Effect.none
                    )

                UserUpdateApiResponded (Ok _) ->
                    ( model
                    , Effect.none
                    )

        Nothing ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view _ =
    { title = "Profile"
    , body = [ viewBody ]
    }


viewBody : Html.Html Msg
viewBody =
    Html.div
        [ Attr.class "settings-page"
        ]
        [ Html.div
            [ Attr.class "container page"
            ]
            [ Html.div
                [ Attr.class "row"
                ]
                [ Html.div
                    [ Attr.class "col-md-6 offset-md-3 col-xs-12"
                    ]
                    [ Html.h1
                        [ Attr.class "text-xs-center"
                        ]
                        [ Html.text "Your Settings" ]
                    , Html.form [ Html.Events.onSubmit UserSubmittedForm ]
                        [ Html.fieldset []
                            [ Html.fieldset
                                [ Attr.class "form-group"
                                ]
                                [ Html.input
                                    [ Attr.class "form-control"
                                    , Attr.type_ "text"
                                    , Attr.placeholder "URL of profile picture"
                                    , Html.Events.onInput (UserUpdatedInput Image)
                                    ]
                                    []
                                ]
                            , Html.fieldset
                                [ Attr.class "form-group"
                                ]
                                [ Html.input
                                    [ Attr.class "form-control form-control-lg"
                                    , Attr.type_ "text"
                                    , Attr.placeholder "Your Name"
                                    , Html.Events.onInput (UserUpdatedInput UserName)
                                    ]
                                    []
                                ]
                            , Html.fieldset
                                [ Attr.class "form-group"
                                ]
                                [ Html.textarea
                                    [ Attr.class "form-control form-control-lg"
                                    , Attr.rows 8
                                    , Attr.placeholder "Short bio about you"
                                    , Html.Events.onInput (UserUpdatedInput Bio)
                                    ]
                                    []
                                ]
                            , Html.fieldset
                                [ Attr.class "form-group"
                                ]
                                [ Html.input
                                    [ Attr.class "form-control form-control-lg"
                                    , Attr.type_ "text"
                                    , Attr.placeholder "Email"
                                    , Html.Events.onInput (UserUpdatedInput Email)
                                    ]
                                    []
                                ]
                            , Html.fieldset
                                [ Attr.class "form-group"
                                ]
                                [ Html.input
                                    [ Attr.class "form-control form-control-lg"
                                    , Attr.type_ "password"
                                    , Attr.placeholder "Password"
                                    , Html.Events.onInput (UserUpdatedInput Password)
                                    ]
                                    []
                                ]
                            , Html.button
                                [ Attr.class "btn btn-lg btn-primary pull-xs-right"
                                ]
                                [ Html.text "Update Settings" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]



-- Form


type Field
    = Email
    | UserName
    | Image
    | Bio
    | Password


type alias FormError =
    { field : Maybe Field
    , message : String
    }


callUserPutApi :
    { username : String
    , image : String
    , email : String
    , bio : Maybe String
    , token : String
    }
    -> Cmd Msg
callUserPutApi user =
    let
        json : Json.Encode.Value
        json =
            Json.Encode.object
                [ ( "user"
                  , Json.Encode.object
                        [ ( "username", Json.Encode.string user.username )
                        , ( "image", Json.Encode.string user.image )
                        , ( "email", Json.Encode.string user.email )
                        , ( "bio", Json.Encode.string (Maybe.withDefault "" user.bio) )
                        ]
                  )
                ]
    in
    Http.request
        { method = "PUT"
        , url = "https://api.realworld.io/api/user"
        , body = Http.jsonBody json
        , expect = expectApiResponse UserUpdateApiResponded Api.User.userDecoder
        , timeout = Nothing
        , tracker = Nothing
        , headers = [ Http.header "Authorization" ("Bearer " ++ user.token) ]
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
                "email" ->
                    Just Email

                "username" ->
                    Just UserName

                "image" ->
                    Just Image

                "bio" ->
                    Just Bio

                "password" ->
                    Just Password

                _ ->
                    Nothing
    in
    Json.Decode.field "errors" (Json.Decode.list formErrorDecoder)


clearErrorsFor : Field -> List FormError -> List FormError
clearErrorsFor field errors =
    errors
        |> List.filter (\error -> error.field /= Just field)
