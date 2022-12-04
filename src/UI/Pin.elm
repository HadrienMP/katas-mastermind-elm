module UI.Pin exposing (display, slot)

import Css
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Pin exposing (Pin)


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
                                Pin.Red ->
                                    "#e32227"

                                Pin.Blue ->
                                    "#0476d0"

                                Pin.Green ->
                                    "#008080"

                                Pin.Black ->
                                    "#000000"

                                Pin.White ->
                                    "#f5f5f5"

                                Pin.Purple ->
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
