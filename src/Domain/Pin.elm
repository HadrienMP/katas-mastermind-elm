module Domain.Pin exposing (KeyPin(..), Pin(..), SecretPin(..), all, equal, openKey, openSecret)


type KeyPin
    = KeyPin Pin


openKey : KeyPin -> Pin
openKey (KeyPin it) =
    it


type SecretPin
    = SecretPin Pin


openSecret : SecretPin -> Pin
openSecret (SecretPin it) =
    it


equal : KeyPin -> SecretPin -> Bool
equal (KeyPin a) (SecretPin b) =
    a == b


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
