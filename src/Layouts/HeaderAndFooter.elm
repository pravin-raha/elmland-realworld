module Layouts.HeaderAndFooter exposing (layout)

import Html exposing (..)
import Html.Attributes as Attr
import Shared exposing (SignInStatus(..))
import View exposing (View)


layout : { page : View msg } -> View msg
layout { page } =
    { title = page.title
    , body =
        [ Html.div [ Attr.class "layout" ]
            [ navbar
            , Html.div [ Attr.class "page" ] page.body
            , footerView
            ]
        ]
    }


navbar : Html msg
navbar =
    nav
        [ Attr.class "navbar navbar-light"
        ]
        [ div
            [ Attr.class "container"
            ]
            [ a
                [ Attr.class "navbar-brand"
                , Attr.href "/"
                ]
                [ text "conduit" ]
            , ul
                [ Attr.class "nav navbar-nav pull-xs-right"
                ]
                navBarLinksView
            ]
        ]


navBarLinksView : List (Html msg)
navBarLinksView =
    li
        [ Attr.class "nav-item"
        ]
        [ {- Add "active" class when you're on that page" -}
          a
            [ Attr.class "nav-link active"
            , Attr.href "/"
            ]
            [ text "Home" ]
        ]
        :: (signedInNavbar ++ signedOutNavbar)


signedInNavbar : List (Html msg)
signedInNavbar =
    [ li
        [ Attr.class "nav-item"
        ]
        [ a
            [ Attr.class "nav-link"
            , Attr.href "/editor"
            ]
            [ i
                [ Attr.class "ion-compose"
                ]
                []
            , text "New Article"
            ]
        ]
    , li
        [ Attr.class "nav-item"
        ]
        [ a
            [ Attr.class "nav-link"
            , Attr.href "/settings"
            ]
            [ i
                [ Attr.class "ion-gear-a"
                ]
                []
            , text "Settings"
            ]
        ]
    ]


signedOutNavbar : List (Html msg)
signedOutNavbar =
    [ li
        [ Attr.class "nav-item"
        ]
        [ a
            [ Attr.class "nav-link"
            , Attr.href "/login"
            ]
            [ text "Sign in" ]
        ]
    , li
        [ Attr.class "nav-item"
        ]
        [ a
            [ Attr.class "nav-link"
            , Attr.href "/register"
            ]
            [ text "Sign up" ]
        ]
    ]


footerView : Html msg
footerView =
    footer []
        [ div
            [ Attr.class "container"
            ]
            [ a
                [ Attr.href "/"
                , Attr.class "logo-font"
                ]
                [ text "conduit" ]
            , span
                [ Attr.class "attribution"
                ]
                [ text "An interactive learning project from"
                , a
                    [ Attr.href "https://thinkster.io"
                    ]
                    [ text "Thinkster" ]
                , text ". Code & design licensed under MIT."
                ]
            ]
        ]
