module UI.Result exposing (rightPlace, wrongPlace)

import Css
import Html.Styled as Html
import Html.Styled.Attributes as Attr


rightPlace : Html.Html msg
rightPlace =
    Html.div
        [ Attr.class "right-place"
        , Attr.css ((Css.backgroundColor <| Css.hex "#f00") :: resultPinStyle)
        ]
        []


wrongPlace : Html.Html msg
wrongPlace =
    Html.div
        [ Attr.class "wrong-place"
        , Attr.css ((Css.backgroundColor <| Css.hex "#fff") :: resultPinStyle)
        ]
        []


resultPinStyle : List Css.Style
resultPinStyle =
    [ Css.width <| Css.rem 0.5
    , Css.height <| Css.rem 0.5
    , Css.borderRadius <| Css.pct 50
    , Css.border3 (Css.px 1) Css.solid <| Css.hex "#000000"
    ]
