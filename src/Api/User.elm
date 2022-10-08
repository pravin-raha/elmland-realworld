module Api.User exposing (User, getCurrentUser)

import Http exposing (emptyBody, header)
import Json.Decode


type alias User =
    { id : Int
    , name : String
    , profileImageUrl : String
    , email : String
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
    Json.Decode.map4 User
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "profileImageUrl" Json.Decode.string)
        (Json.Decode.field "email" Json.Decode.string)
