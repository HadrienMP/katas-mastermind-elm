module UI.Flex exposing
    ( Rem(..)
    , cssGap
    , defaultGap
    )

import Css


defaultGap : Css.Style
defaultGap =
    cssGap <| Rem 1


type Rem
    = Rem Float


cssGap : Rem -> Css.Style
cssGap (Rem it) =
    Css.property "gap" <| String.fromFloat it ++ "rem"
