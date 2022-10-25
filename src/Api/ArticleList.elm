module Api.ArticleList exposing (Article, getFirst20, getFirst20Feeds, toUserFriendlyMessage)

import Effect exposing (Effect)
import Http
import Json.Decode exposing (..)


type alias Author =
    { username : String
    , image : String
    }


type alias Article =
    { title : String
    , body : String
    , updatedAt : String
    , favoritesCount : Int
    , author : Author
    , tagList : List String
    }


getFirst20 :
    { onResponse : Result Http.Error (List Article) -> msg
    }
    -> Effect msg
getFirst20 options =
    Effect.fromCmd
        (Http.get
            { url = "https://api.realworld.io/api/articles?limit=20&offset=0"
            , expect = Http.expectJson options.onResponse decoder
            }
        )


getFirst20Feeds :
    { onResponse : Result Http.Error (List Article) -> msg
    , token : String
    }
    -> Effect msg
getFirst20Feeds options =
    Effect.fromCmd
        (Http.request
            { method = "GET"
            , url = "https://api.realworld.io/api/articles/feeds?limit=20&offset=0"
            , expect = Http.expectJson options.onResponse decoder
            , body = Http.emptyBody
            , timeout = Nothing
            , tracker = Nothing
            , headers = [ Http.header "Authorization" ("Bearer " ++ options.token) ]
            }
        )

decoder : Json.Decode.Decoder (List Article)
decoder =
    Json.Decode.field "articles" (Json.Decode.list articleDecoder)


articleDecoder : Json.Decode.Decoder Article
articleDecoder =
    Json.Decode.map6 Article
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "body" Json.Decode.string)
        (Json.Decode.field "updatedAt" Json.Decode.string)
        (Json.Decode.field "favoritesCount" Json.Decode.int)
        (Json.Decode.field "author" authorDecoder)
        (Json.Decode.field "tagList" (Json.Decode.list Json.Decode.string))


authorDecoder : Json.Decode.Decoder Author
authorDecoder =
    Json.Decode.map2 Author
        (Json.Decode.field "username" Json.Decode.string)
        (Json.Decode.field "image" Json.Decode.string)

toUserFriendlyMessage : Http.Error -> String
toUserFriendlyMessage httpError =
    case httpError of
        Http.BadUrl _ ->
            -- The URL is malformed, probably caused by a typo
            "This page requested a bad URL"

        Http.Timeout ->
            -- Happens after
            "Request took too long to respond"

        Http.NetworkError ->
            -- Happens if the user is offline or the API isn't online
            "Could not connect to the API"

        Http.BadStatus code ->
            -- Connected to the API, but something went wrong
            if code == 404 then
                "Item not found"

            else
                "API returned an error code:" ++  String.fromInt code

        Http.BadBody _ ->
            -- Our JSON decoder didn't match what the API sent
            "Unexpected response from API"