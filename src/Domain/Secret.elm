module Domain.Secret exposing
    ( Secret(..)
    , append
    , empty
    , open
    )

import Domain.Pin exposing (Pin)


type Secret
    = Secret (List Pin)


append : Pin -> Secret -> Secret
append pin (Secret secret) =
    Secret <| secret ++ [ pin ]


empty : Secret
empty =
    Secret []


open : Secret -> List Pin
open (Secret secret) =
    secret
