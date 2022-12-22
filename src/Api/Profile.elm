module Api.Profile exposing (followUser, getProfile, unFollowUser, Profile)

import Effect exposing (Effect)
import Http
import Json.Decode exposing (bool, maybe, string)
import Json.Decode.Pipeline exposing (required)


type alias Profile =
    { username : String
    , bio : Maybe String
    , image : String
    , following : Bool
    }


followUser :
    { onResponse : Result Http.Error Profile -> msg
    , token : Maybe String
    , username : String
    }
    -> Effect msg
followUser options =
    let
        headers =
            case options.token of
                Just token ->
                    [ Http.header "Authorization" ("Bearer " ++ token) ]

                Nothing ->
                    []
    in
    Effect.sendCmd
        (Http.request
            { method = "POST"
            , url = "https://api.realworld.io/api/profiles/" ++ options.username ++ "/follow"
            , expect = Http.expectJson options.onResponse profileDecoder
            , body = Http.emptyBody
            , timeout = Nothing
            , tracker = Nothing
            , headers = headers
            }
        )


unFollowUser :
    { onResponse : Result Http.Error Profile -> msg
    , token : Maybe String
    , username : String
    }
    -> Effect msg
unFollowUser options =
    let
        headers =
            case options.token of
                Just token ->
                    [ Http.header "Authorization" ("Bearer " ++ token) ]

                Nothing ->
                    []
    in
    Effect.sendCmd
        (Http.request
            { method = "DELETE"
            , url = "https://api.realworld.io/api/profiles/" ++ options.username ++ "/follow"
            , expect = Http.expectJson options.onResponse profileDecoder
            , body = Http.emptyBody
            , timeout = Nothing
            , tracker = Nothing
            , headers = headers
            }
        )


profileDecoder : Json.Decode.Decoder Profile
profileDecoder =
    Json.Decode.field "profile"
        (Json.Decode.succeed Profile
            |> required "username" string
            |> required "bio" (maybe string)
            |> required "image" string
            |> required "following" bool
        )


getProfile :
    { onResponse : Result Http.Error Profile -> msg
    , username : String
    }
    -> Effect msg
getProfile { onResponse, username } =
    Effect.sendCmd
        (Http.get
            { url = "https://api.realworld.io/api/profiles/" ++ username
            , expect = Http.expectJson onResponse profileDecoder
            }
        )
