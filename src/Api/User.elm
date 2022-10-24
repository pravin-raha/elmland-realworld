module Api.User exposing (User, getCurrentUser)

import Http exposing (emptyBody, header)
import Json.Decode
import Json.Decode as Decode exposing (maybe, string)
import Json.Decode.Pipeline exposing (required)


type alias User =
    { username : String
    , image : String
    , email : String
    , token : String
    , bio : Maybe String
    }


getCurrentUser :
    { token : String
    , onResponse : Result Http.Error User -> msg
    }
    -> Cmd msg
getCurrentUser { token, onResponse } =
    Http.request
        { method = "GET"
        , url = "https://api.realworld.io/api/user"
        , headers = [ header "Authorization" ("Bearer " ++ token) ]
        , expect = Http.expectJson onResponse userDecoder
        , body = emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


userDecoder : Json.Decode.Decoder User
userDecoder =
    Json.Decode.field "user"
        (Decode.succeed User
            |> required "username" string
            |> required "image" string
            |> required "email" string
            |> required "token" string
            |> required "bio" (maybe string)
        )
