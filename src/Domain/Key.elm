module Domain.Key exposing (Key(..), append, empty)

import Domain.Pin exposing (Pin)


type Key
    = Key (List Pin)


append : Pin -> Key -> Key
append pin (Key secret) =
    Key <| secret ++ [ pin ]


empty : Key
empty =
    Key []
