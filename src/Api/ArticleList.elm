module Api.ArticleList exposing (Article, getFirst20)

import Effect exposing (Effect)
import Http
import Json.Decode


type alias Author =
    { username : String
    , image : String
    }


type alias Article =
    { title : String
    , body : String
    , updatedAt : String
    , favoritesCount : Int
    , author: Author
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


decoder : Json.Decode.Decoder (List Article)
decoder =
    Json.Decode.field "articles" (Json.Decode.list articleDecoder)


articleDecoder : Json.Decode.Decoder Article
articleDecoder =
    Json.Decode.map5 Article
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "body" Json.Decode.string)
        (Json.Decode.field "updatedAt" Json.Decode.string)
        (Json.Decode.field "favoritesCount" Json.Decode.int)
        (Json.Decode.field "author" authorDecoder)

authorDecoder : Json.Decode.Decoder Author
authorDecoder =
    Json.Decode.map2 Author
        (Json.Decode.field "username" Json.Decode.string)
        (Json.Decode.field "image" Json.Decode.string)
        
