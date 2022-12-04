module KeyInput exposing (KeyInput, clear, empty, parse, pins, put)

import Core exposing (Key)
import Dict exposing (Dict)
import Pin


type alias Internal =
    { size : Int, content : Dict Int Pin.Pin }


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


contentOf : KeyInput -> Dict Int Pin.Pin
contentOf =
    open >> .content


put : { position : Int, pin : Maybe Pin.Pin } -> KeyInput -> KeyInput
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
                        Just <| it :: list

                    ( Nothing, _ ) ->
                        Nothing

                    ( _, Nothing ) ->
                        Nothing
            )
            (Just [])
        |> Maybe.map Core.Key


clear : KeyInput -> KeyInput
clear (KeyInput internal) =
    KeyInput { internal | content = Dict.empty }


pins : KeyInput -> List ( Int, Maybe Pin.Pin )
pins (KeyInput internal) =
    List.range 0 (internal.size - 1)
        |> List.map (\index -> ( index, Dict.get index internal.content ))
