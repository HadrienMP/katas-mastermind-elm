module UI.Pin exposing (display, slot)

import Css
import Domain.Pin exposing (Pin(..))
import Html.Styled as Html
import Html.Styled.Attributes as Attr


display : Pin -> Html.Html msg
display current =
    Html.div
        [ Attr.class "pin"
        , Attr.css
            (commonStyle
                ++ [ Css.border3 (Css.px 1) Css.solid <| Css.hex "#000000"
                   , Css.backgroundColor <|
                        Css.hex <|
                            case current of
                                Red ->
                                    "#e32227"

                                Blue ->
                                    "#0476d0"

                                Green ->
                                    "#008080"

                                Black ->
                                    "#000000"

                                White ->
                                    "#f5f5f5"

                                Purple ->
                                    "#ff00ff"
                   ]
            )
        ]
        []


slot : Html.Html msg
slot =
    Html.div
        [ Attr.class "pin-slot"
        , Attr.css ((Css.backgroundColor <| Css.hex "#ccc") :: commonStyle)
        ]
        []


commonStyle : List Css.Style
commonStyle =
    [ Css.width <| Css.rem 2
    , Css.height <| Css.rem 2
    , Css.borderRadius <| Css.pct 50
    , Css.boxSizing Css.borderBox
    ]
