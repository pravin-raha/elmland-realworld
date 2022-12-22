module Shared.Model exposing (Model, SignInStatus(..), User)

{-| -}

import Http


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type alias Model =
    { signInStatus : SignInStatus
    }


type SignInStatus
    = NotSignedIn
    | SignedInWithToken String
    | SignedInWithUser User
    | FailedToSignIn Http.Error

type alias User =
    { username : String
    , image : String
    , email : String
    , token : String
    , bio : Maybe String
    }