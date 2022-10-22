module Pages.Register exposing (Model, Msg, page)

import Api.User
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events
import Http
import Json.Decode
import Json.Encode
import Layout exposing (Layout)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Msg
import View exposing (View)


layout : Layout
layout =
    Layout.HeaderAndFooter


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { email : String
    , password : String
    , username : String
    , errors : List FormError
    , isSubmittingForm : Bool
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { email = ""
      , password = ""
      , username = ""
      , errors = []
      , isSubmittingForm = False
      }
    , Effect.none
    )



-- UPDATE


type alias RegisterUser =
    { username : String
    , email : String
    , password : String
    }


type alias RegisterUserResponse =
    { username : String
    , token : String
    , email : String
    , bio : String
    , image : String
    }


type Field
    = Email
    | Password
    | UserName


type alias FormError =
    { field : Maybe Field
    , message : String
    }


type Msg
    = RegisterUserApiResponded (Result (List FormError) String)
    | UserApiResponded (Result Http.Error Api.User.User)
    | UserUpdatedInput Field String
    | UserSubmittedForm


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        UserSubmittedForm ->
            ( { model
                | isSubmittingForm = True
                , errors = []
              }
            , Effect.fromCmd
                (callRegisterApi
                    { username = model.username
                    , email = model.email
                    , password = model.password
                    }
                )
            )

        RegisterUserApiResponded (Err formErrors) ->
            ( { model | errors = formErrors, isSubmittingForm = False }
            , Effect.none
            )

        RegisterUserApiResponded (Ok token) ->
            ( model
            , Effect.batch
                [ Effect.save
                    { key = "token"
                    , value = Json.Encode.string token
                    }
                , Effect.fromCmd
                    (Api.User.getCurrentUser
                        { token = token
                        , onResponse = UserApiResponded
                        }
                    )
                ]
            )

        UserApiResponded result ->
            ( model
            , Effect.fromSharedMsg (Shared.Msg.SignInPageSignedInUser result)
            )

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

        UserUpdatedInput Password value ->
            ( { model
                | password = value
                , errors = clearErrorsFor Password model.errors
              }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Register"
    , body = [ viewBody ]
    }


viewBody : Html Msg
viewBody =
    div
        [ Attr.class "auth-page"
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
                        [ text "Sign up" ]
                    , p
                        [ Attr.class "text-xs-center"
                        ]
                        [ a
                            [ Attr.href ""
                            ]
                            [ text "Have an account?" ]
                        ]
                    , ul
                        [ Attr.class "error-messages"
                        ]
                        [ li []
                            [ text "That email is already taken" ]
                        ]
                    , form [ Html.Events.onSubmit UserSubmittedForm ]
                        [ fieldset
                            [ Attr.class "form-group"
                            ]
                            [ input
                                [ Attr.class "form-control form-control-lg"
                                , Attr.type_ "text"
                                , Attr.placeholder "Your Name"
                                , Attr.required True
                                , Html.Events.onInput (UserUpdatedInput UserName)
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
                                , Attr.required True
                                , Html.Events.onInput (UserUpdatedInput Email)
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
                                , Attr.required True
                                , Html.Events.onInput (UserUpdatedInput Password)
                                ]
                                []
                            ]
                        , button
                            [ Attr.class "btn btn-lg btn-primary pull-xs-right"
                            ]
                            [ text "Sign up" ]
                        ]
                    ]
                ]
            ]
        ]



-- Api Call


callRegisterApi : RegisterUser -> Cmd Msg
callRegisterApi registerUser =
    let
        json : Json.Encode.Value
        json =
            Json.Encode.object
                [ ( "user"
                  , Json.Encode.object
                        [ ( "username", Json.Encode.string registerUser.username )
                        , ( "email", Json.Encode.string registerUser.email )
                        , ( "password", Json.Encode.string registerUser.password )
                        ]
                  )
                ]

        userDecoder =
            Json.Decode.field "user" tokenDecoder

        tokenDecoder : Json.Decode.Decoder String
        tokenDecoder =
            Json.Decode.field "token" Json.Decode.string
    in
    Http.post
        { url = "https://api.realworld.io/api/users"
        , body = Http.jsonBody json
        , expect = expectApiResponse RegisterUserApiResponded userDecoder
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

                "password" ->
                    Just Password

                "username" ->
                    Just UserName

                _ ->
                    Nothing
    in
    Json.Decode.field "errors" (Json.Decode.list formErrorDecoder)


clearErrorsFor : Field -> List FormError -> List FormError
clearErrorsFor field errors =
    errors
        |> List.filter (\error -> error.field /= Just field)
