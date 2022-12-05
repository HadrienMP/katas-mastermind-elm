module Domain.Key exposing (Key(..))

import Domain.Pin exposing (Pin)


type Key
    = Key (List Pin)
