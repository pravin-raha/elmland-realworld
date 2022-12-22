module Auth exposing (User, onPageLoad)

import Api.User
import Auth.Action
import Dict
import Route exposing (Route)
import Route.Path
import Shared.Model exposing (SignInStatus(..))


type alias User =
    Maybe Api.User.User


onPageLoad : Shared.Model.Model -> Route () -> Auth.Action.Action User
onPageLoad shared route =
    case shared.signInStatus of
        NotSignedIn ->
            Auth.Action.loadPageWithUser Nothing

        SignedInWithToken token ->
            Auth.Action.showLoadingPage
                { title = "Signing in..."
                , body =
                    []
                }

        SignedInWithUser user ->
            Auth.Action.loadPageWithUser (Just user)

        FailedToSignIn user ->
            Auth.Action.pushRoute
                { path = Route.Path.Login
                , query = Dict.fromList [ ( "from", route.url.path ) ]
                , hash = Nothing
                }
