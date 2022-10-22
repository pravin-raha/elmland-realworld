module Pages.Login exposing (Model, Msg, page)

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
    , errors : List FormError
    , isSubmittingForm : Bool
    }


type Field
    = Email
    | Password


type alias FormError =
    { field : Maybe Field
    , message : String
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { email = ""
      , password = ""
      , errors = []
      , isSubmittingForm = False
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = UserUpdatedInput Field String
    | UserSubmittedForm
    | SignInApiResponded (Result (List FormError) String)
    | UserApiResponded (Result Http.Error Api.User.User)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        UserUpdatedInput Email value ->
            ( { model
                | email = value
                , errors = clearErrorsFor Email model.errors
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

        UserSubmittedForm ->
            ( { model
                | isSubmittingForm = True
                , errors = []
              }
            , Effect.fromCmd
                (callSignInApi
                    { email = model.email
                    , password = model.password
                    }
                )
            )

        SignInApiResponded (Err formErrors) ->
            ( { model | errors = formErrors, isSubmittingForm = False }
            , Effect.none
            )

        SignInApiResponded (Ok token) ->
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


clearErrorsFor : Field -> List FormError -> List FormError
clearErrorsFor field errors =
    errors
        |> List.filter (\error -> error.field /= Just field)


callSignInApi : { email : String, password : String } -> Cmd Msg
callSignInApi form =
    let
        json : Json.Encode.Value
        json =
            Json.Encode.object
                [ ( "user"
                  , Json.Encode.object
                        [ ( "email", Json.Encode.string form.email )
                        , ( "password", Json.Encode.string form.password )
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
        { url = "https://api.realworld.io/api/users/login"
        , body = Http.jsonBody json
        , expect = expectApiResponse SignInApiResponded userDecoder
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

                _ ->
                    Nothing
    in
    Json.Decode.field "errors" (Json.Decode.list formErrorDecoder)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "login"
    , body = [ viewBody ]
    }


viewBody : Html Msg
viewBody =
    let
        viewFormInput : List (Html Msg)
        viewFormInput =
            [ fieldset
                [ Attr.class "form-group"
                ]
                [ input
                    [ Attr.class "form-control form-control-lg"
                    , Attr.type_ "text"
                    , Attr.placeholder "Email"
                    , Html.Events.onInput (UserUpdatedInput Email)
                    , Attr.required True
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
                    , Html.Events.onInput (UserUpdatedInput Password)
                    , Attr.required True
                    ]
                    []
                ]
            , button
                [ Attr.class "btn btn-lg btn-primary pull-xs-right"
                ]
                [ text "Sign up" ]
            ]
    in
    Html.div [ Attr.class "auth-page" ]
        [ Html.div [ Attr.class "container page" ]
            [ Html.div [ Attr.class "row" ]
                [ Html.div [ Attr.class "col-md-6 offset-md-3 col-xs-12" ]
                    [ Html.h1 [ Attr.class "text-xs-center" ] [ Html.text "Sign in" ]
                    , Html.p [ Attr.class "text-xs-center" ] [ Html.a [ Attr.href "" ] [ Html.text "Need an account?" ] ]
                    , Html.form [ Html.Events.onSubmit UserSubmittedForm ] viewFormInput
                    ]
                ]
            ]
        ]
