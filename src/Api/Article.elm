module Api.Article exposing
    ( Article
    , Comment
    , deleteArticleApi
    , favoriteArticleCommets
    , getArticle
    , getArticleCommets
    , getFirst20ArticleBy
    , getFirst20Feeds
    , singleArticleCommentDecoder
    , toUserFriendlyMessage
    , unfavoriteArticleCommets
    )

import Effect exposing (Effect)
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline
import Url.Builder


type alias Author =
    { username : String
    , image : String
    , following : Bool
    }


type alias Article =
    { title : String
    , body : String
    , updatedAt : String
    , favoritesCount : Int
    , author : Author
    , tagList : List String
    , description : String
    , slug : String
    , favorited : Bool
    }


getFirst20ArticleBy :
    { onResponse : Result Http.Error (List Article) -> msg
    , author : Maybe String
    , favorited : Maybe String
    , tag : Maybe String
    , token : Maybe String
    }
    -> Effect msg
getFirst20ArticleBy options =
    let
        headers =
            case options.token of
                Just token ->
                    [ Http.header "Authorization" ("Bearer " ++ token) ]

                Nothing ->
                    []

        autherParam =
            Maybe.withDefault [] (Maybe.map (\a -> [ Url.Builder.string "author" a ]) options.author)

        favoritedParam =
            Maybe.withDefault [] (Maybe.map (\f -> [ Url.Builder.string "favorited" f ]) options.favorited)

        tagParam =
            Maybe.withDefault [] (Maybe.map (\f -> [ Url.Builder.string "tag" f ]) options.tag)

        params =
            autherParam ++ favoritedParam ++ tagParam ++ [ Url.Builder.int "limit" 20, Url.Builder.int "offset" 0 ]

        url =
            "https://api.realworld.io/" ++ Url.Builder.relative [ "api", "articles" ] params
    in
    Effect.sendCmd
        (Http.request
            { method = "GET"
            , url = url
            , expect = Http.expectJson options.onResponse articleListdecoder
            , body = Http.emptyBody
            , timeout = Nothing
            , tracker = Nothing
            , headers = headers
            }
        )


getFirst20Feeds :
    { onResponse : Result Http.Error (List Article) -> msg
    , token : String
    }
    -> Effect msg
getFirst20Feeds options =
    Effect.sendCmd
        (Http.request
            { method = "GET"
            , url = "https://api.realworld.io/api/articles/feeds?limit=20&offset=0"
            , expect = Http.expectJson options.onResponse articleListdecoder
            , body = Http.emptyBody
            , timeout = Nothing
            , tracker = Nothing
            , headers = [ Http.header "Authorization" ("Bearer " ++ options.token) ]
            }
        )


articleListdecoder : Json.Decode.Decoder (List Article)
articleListdecoder =
    Json.Decode.field "articles" (Json.Decode.list articleDecoder)


articleDecoder : Json.Decode.Decoder Article
articleDecoder =
    Json.Decode.succeed Article
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "body" Json.Decode.string
        |> Json.Decode.Pipeline.required "updatedAt" Json.Decode.string
        |> Json.Decode.Pipeline.required "favoritesCount" Json.Decode.int
        |> Json.Decode.Pipeline.required "author" authorDecoder
        |> Json.Decode.Pipeline.required "tagList" (Json.Decode.list Json.Decode.string)
        |> Json.Decode.Pipeline.required "description" Json.Decode.string
        |> Json.Decode.Pipeline.required "slug" Json.Decode.string
        |> Json.Decode.Pipeline.required "favorited" Json.Decode.bool


singleArticleDecoder : Json.Decode.Decoder Article
singleArticleDecoder =
    Json.Decode.field "article" articleDecoder


authorDecoder : Json.Decode.Decoder Author
authorDecoder =
    Json.Decode.succeed Author
        |> Json.Decode.Pipeline.required "username" Json.Decode.string
        |> Json.Decode.Pipeline.required "image" Json.Decode.string
        |> Json.Decode.Pipeline.required "following" Json.Decode.bool


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
                "API returned an error code:" ++ String.fromInt code

        Http.BadBody _ ->
            -- Our JSON decoder didn't match what the API sent
            "Unexpected response from API"


getArticle :
    { onResponse : Result Http.Error Article -> msg
    , token : Maybe String
    , slug : String
    }
    -> Effect msg
getArticle options =
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
            { method = "GET"
            , url = "https://api.realworld.io/api/articles/" ++ options.slug
            , expect = Http.expectJson options.onResponse singleArticleDecoder
            , body = Http.emptyBody
            , timeout = Nothing
            , tracker = Nothing
            , headers = headers
            }
        )



-- Comments


type alias Comment =
    { id : Int
    , createdAt : String
    , updatedAt : String
    , body : String
    , author : Author
    }


getArticleCommets :
    { onResponse : Result Http.Error (List Comment) -> msg
    , token : Maybe String
    , slug : String
    }
    -> Effect msg
getArticleCommets options =
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
            { method = "GET"
            , url = "https://api.realworld.io/api/articles/" ++ options.slug ++ "/comments"
            , expect = Http.expectJson options.onResponse articleCommnetListdecoder
            , body = Http.emptyBody
            , timeout = Nothing
            , tracker = Nothing
            , headers = headers
            }
        )


articleCommnetListdecoder : Json.Decode.Decoder (List Comment)
articleCommnetListdecoder =
    Json.Decode.field "comments" (Json.Decode.list articleCommentDecoder)


singleArticleCommentDecoder : Json.Decode.Decoder Comment
singleArticleCommentDecoder =
    Json.Decode.field "comment" articleCommentDecoder


articleCommentDecoder : Json.Decode.Decoder Comment
articleCommentDecoder =
    Json.Decode.succeed Comment
        |> Json.Decode.Pipeline.required "id" Json.Decode.int
        |> Json.Decode.Pipeline.required "createdAt" Json.Decode.string
        |> Json.Decode.Pipeline.required "updatedAt" Json.Decode.string
        |> Json.Decode.Pipeline.required "body" Json.Decode.string
        |> Json.Decode.Pipeline.required "author" authorDecoder


favoriteArticleCommets :
    { onResponse : Result Http.Error Article -> msg
    , token : Maybe String
    , slug : String
    }
    -> Effect msg
favoriteArticleCommets options =
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
            , url = "https://api.realworld.io/api/articles/" ++ options.slug ++ "/favorite"
            , expect = Http.expectJson options.onResponse singleArticleDecoder
            , body = Http.emptyBody
            , timeout = Nothing
            , tracker = Nothing
            , headers = headers
            }
        )


unfavoriteArticleCommets :
    { onResponse : Result Http.Error Article -> msg
    , token : Maybe String
    , slug : String
    }
    -> Effect msg
unfavoriteArticleCommets options =
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
            , url = "https://api.realworld.io/api/articles/" ++ options.slug ++ "/favorite"
            , expect = Http.expectJson options.onResponse singleArticleDecoder
            , body = Http.emptyBody
            , timeout = Nothing
            , tracker = Nothing
            , headers = headers
            }
        )


deleteArticleApi :
    { onResponse : Result Http.Error String -> msg
    , slug : String
    , token : String
    }
    -> Effect msg
deleteArticleApi payload =
    Effect.sendCmd
        (Http.request
            { method = "DELETE"
            , url = "https://api.realworld.io/api/articles/" ++ payload.slug
            , body = Http.emptyBody
            , expect = Http.expectString payload.onResponse
            , headers = [ Http.header "Authorization" ("Bearer " ++ payload.token) ]
            , timeout = Nothing
            , tracker = Nothing
            }
        )
