module Domain.Code exposing (Key, Secret)

import Domain.Pin exposing (KeyPin, SecretPin)


type alias Key =
    List KeyPin


type alias Secret =
    List SecretPin
