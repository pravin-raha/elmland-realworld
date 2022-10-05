module Layouts.NavBar exposing (layout)

import Html exposing (..)
import Html.Attributes as Attr
import View exposing (View)


layout : { page : View msg } -> View msg
layout { page } =
    { title = page.title
    , body =
        [ Html.div [ Attr.class "layout" ]
            [ viewSidebar
            , Html.div [ Attr.class "page" ] page.body
            ]
        ]
    }


viewSidebar : Html msg
viewSidebar =
    nav
        [ Attr.class "navbar navbar-light"
        ]
        [ div
            [ Attr.class "container"
            ]
            [ a
                [ Attr.class "navbar-brand"
                , Attr.href "index.html"
                ]
                [ text "conduit" ]
            , ul
                [ Attr.class "nav navbar-nav pull-xs-right"
                ]
                [ li
                    [ Attr.class "nav-item"
                    ]
                    [ {- Add "active" class when you're on that page" -}
                      a
                        [ Attr.class "nav-link active"
                        , Attr.href "/"
                        ]
                        [ text "Home" ]
                    ]
                , li
                    [ Attr.class "nav-item"
                    ]
                    [ a
                        [ Attr.class "nav-link"
                        , Attr.href ""
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
                        , Attr.href ""
                        ]
                        [ i
                            [ Attr.class "ion-gear-a"
                            ]
                            []
                        , text "Settings"
                        ]
                    ]
                , li
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
            ]
        ]
