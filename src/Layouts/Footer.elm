module Layouts.Footer exposing (layout)

import Html exposing (..)
import Html.Attributes as Attr
import View exposing (View)


layout : { page : View msg } -> View msg
layout { page } =
    { title = page.title
    , body =
        [ Html.div [ Attr.class "layout" ]
            [ footerView
            , Html.div [ Attr.class "page" ] page.body
            ]
        ]
    }


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
                [ text "An interactive learning project from", a
                    [ Attr.href "https://thinkster.io"
                    ]
                    [ text "Thinkster" ]
                , text ". Code & design licensed under MIT." ]
            ]
        ]
    
