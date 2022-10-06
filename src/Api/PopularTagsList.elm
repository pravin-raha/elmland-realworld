module Api.PopularTagsList exposing (getTags)

import Effect exposing (Effect)
import Http
import Json.Decode exposing (..)


getTags :
    { onResponse : Result Http.Error (List String) -> msg
    }
    -> Effect msg
getTags options =
    Effect.fromCmd
        (Http.get
            { url = "https://api.realworld.io/api/tags"
            , expect = Http.expectJson options.onResponse decoder
            }
        )


decoder : Json.Decode.Decoder (List String)
decoder =
    Json.Decode.field "tags" (Json.Decode.list Json.Decode.string)
