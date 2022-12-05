module Domain.Secret exposing
    ( Secret(..)
    , length
    , open
    )

import Domain.Pin exposing (Pin)


type Secret
    = Secret (List Pin)


open : Secret -> List Pin
open (Secret secret) =
    secret


length : Secret -> Int
length =
    open >> List.length
