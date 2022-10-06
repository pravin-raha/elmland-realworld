module Api.ArticleList exposing (getFirst20)

import Effect exposing (Effect)
import Http
import Json.Decode


type alias Article =
    { title : String
    , body : String
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
    Json.Decode.map2 Article
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "body" Json.Decode.string)
