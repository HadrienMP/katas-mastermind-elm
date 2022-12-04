module Domain.Pin exposing (Pin(..), all)


type Pin
    = Red
    | Blue
    | Green
    | Black
    | White
    | Purple


all : List Pin
all =
    next [] |> List.reverse


next : List Pin -> List Pin
next list =
    case List.head list of
        Nothing ->
            next (Red :: list)

        Just Red ->
            next (Blue :: list)

        Just Blue ->
            next (Green :: list)

        Just Green ->
            next (Black :: list)

        Just Black ->
            next (White :: list)

        Just White ->
            next (Purple :: list)

        Just Purple ->
            list
