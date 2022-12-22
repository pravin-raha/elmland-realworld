module Pages.Register exposing (Model, Msg, page)

import Api.User
import Auth
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events
import Http
import Json.Decode 
import Json.Decode as Decode exposing (list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)
import Shared.Msg


layout : Auth.User -> Model -> Layouts.Layout
layout user model =
    Layouts.HeaderAndFooter
        { headerAndFooter =
            { title = "Sign Up"
            , user = user
            }
        }


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (layout Nothing)



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


fieldToString : Field -> String
fieldToString field =
    case field of
        Email ->
            "email"

        Password ->
            "password"

        UserName ->
            "username"


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
    { title = "Sign Up"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
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
                    , formErrorUlView model -- Search a way to do Htmnone
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


formErrorUlView : Model -> Html msg
formErrorUlView model =
    if List.isEmpty model.errors then
        text ""

    else
        ul
            [ Attr.class "error-messages" ]
            (formErrorLiView model)


formErrorLiView : Model -> List (Html msg)
formErrorLiView model =
    let
        toListview : FormError -> Html msg
        toListview fError =
            li []
                [ text ("That " ++ fieldToString (Maybe.withDefault Email fError.field) ++ " " ++ fError.message) ]
    in
    List.map toListview model.errors



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


type alias RegisterUserApiErrorResponse =
    { email : List String
    , username : List String
    }


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
            case statusCode of
                422 ->
                    case Json.Decode.decodeString registerUserApiErrorResponseDecoder rawJson of
                        Ok registerUserApiErrorResponse ->
                            Err (registerUserApiErrorResponseError registerUserApiErrorResponse)

                        Err _ ->
                            Err [ { field = Nothing, message = "Received status code " ++ String.fromInt statusCode } ]

                _ ->
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


stringToFormError : Field -> String -> FormError
stringToFormError field email =
    { field = Just field, message = email }


registerUserApiErrorResponseError : RegisterUserApiErrorResponse -> List FormError
registerUserApiErrorResponseError error =
    let
        emailErr =
            List.map (stringToFormError Email) error.email

        usernameErr =
            List.map (stringToFormError UserName) error.username
    in
    emailErr ++ usernameErr


registerUserApiErrorResponseDecoder : Json.Decode.Decoder RegisterUserApiErrorResponse
registerUserApiErrorResponseDecoder =
    let
        urUserDecoder =
            Decode.succeed RegisterUserApiErrorResponse
                |> required "email" (list string)
                |> required "username" (list string)
    in
    Json.Decode.field "errors" urUserDecoder


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
