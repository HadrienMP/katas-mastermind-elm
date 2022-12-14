module UI.KeyInput exposing (KeyInput, clear, empty, put, view)

import Css
import Dict exposing (Dict)
import Domain.Code exposing (Key)
import Domain.Pin exposing (KeyPin(..), Pin)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import UI.Flex
import UI.Pin


type alias Internal =
    { size : Int, content : Dict Int Pin }


type KeyInput
    = KeyInput Internal


empty : Int -> KeyInput
empty size =
    KeyInput { size = size, content = Dict.empty }


open : KeyInput -> Internal
open (KeyInput internal) =
    internal


sizeOf : KeyInput -> Int
sizeOf =
    open >> .size


contentOf : KeyInput -> Dict Int Pin
contentOf =
    open >> .content


put : { position : Int, pin : Maybe Pin } -> KeyInput -> KeyInput
put { position, pin } input =
    if position < sizeOf input then
        let
            internal =
                open input
        in
        KeyInput
            { internal
                | content =
                    Dict.update position (always pin) (contentOf input)
            }

    else
        input


parse : KeyInput -> Maybe Key
parse input =
    pins input
        |> List.map Tuple.second
        |> List.foldr
            (\el acc ->
                case ( acc, el ) of
                    ( Just list, Just it ) ->
                        Just <| KeyPin it :: list

                    ( Nothing, _ ) ->
                        Nothing

                    ( _, Nothing ) ->
                        Nothing
            )
            (Just [])


clear : KeyInput -> KeyInput
clear (KeyInput internal) =
    KeyInput { internal | content = Dict.empty }


pins : KeyInput -> List ( Int, Maybe Pin )
pins (KeyInput internal) =
    List.range 0 (internal.size - 1)
        |> List.map (\index -> ( index, Dict.get index internal.content ))



-- View


view :
    { onCheck : Key -> msg
    , onSlotClick : Int -> msg
    }
    -> KeyInput
    -> Html msg
view { onCheck, onSlotClick } input =
    Html.div
        [ Attr.css
            [ Css.displayFlex
            , UI.Flex.defaultGap
            ]
        ]
        [ pins input
            |> List.map (selectionPin onSlotClick)
            |> UI.Pin.pinBox
        , Html.button
            (case parse input of
                Just key ->
                    [ Events.onClick <| onCheck key ]

                Nothing ->
                    [ Attr.disabled True ]
            )
            [ Html.text "Check" ]
        ]


selectionPin : (Int -> msg) -> ( Int, Maybe Pin ) -> Html msg
selectionPin onClick ( position, pin ) =
    actionablePinSlot onClick position <|
        case pin of
            Just it ->
                UI.Pin.display it

            Nothing ->
                UI.Pin.slot


actionablePinSlot : (Int -> msg) -> Int -> Html msg -> Html msg
actionablePinSlot onClick position child =
    Html.div [ Events.onClick <| onClick position ] [ child ]
