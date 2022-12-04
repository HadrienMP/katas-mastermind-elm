module UI.Typography exposing (title)

import Css
import Html.Styled as Html
import Html.Styled.Attributes as Attr


title : String -> Html.Html msg
title text =
    Html.h1
        [ Attr.css
            [ Css.textTransform Css.uppercase
            , Css.margin Css.zero
            , Css.fontSize <| Css.rem 2.5
            , Css.textAlign Css.center
            ]
        ]
        [ Html.text text ]
