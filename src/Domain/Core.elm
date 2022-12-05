module Domain.Core exposing
    ( Result
    , countWrongPlace
    , score
    )

import Domain.Key exposing (Key(..))
import Domain.Secret exposing (Secret(..))
import List.Extra


type alias Result =
    { rightPlace : Int, wrongPlace : Int }


score : Secret -> Key -> Result
score secret key =
    let
        ( unmatchedKey, unmatchedSecret ) =
            removeExactMatches key secret
    in
    { rightPlace = Domain.Secret.length secret - Domain.Secret.length unmatchedSecret
    , wrongPlace = countWrongPlace unmatchedSecret unmatchedKey
    }


removeExactMatches : Key -> Secret -> ( Key, Secret )
removeExactMatches (Key key) (Secret secret) =
    List.Extra.zip key secret
        |> List.filter (\( a, b ) -> a /= b)
        |> List.unzip
        |> Tuple.mapBoth Key Secret



-- Wrong place


countWrongPlace : Secret -> Key -> Int
countWrongPlace secret key =
    countWrongPlaceRec 0 secret key


countWrongPlaceRec : Int -> Secret -> Key -> Int
countWrongPlaceRec acc (Secret secret) (Key guess) =
    case guess of
        firstKeyPin :: restOfKey ->
            let
                secretLeft =
                    firstKeyPin |> removeOnceFrom secret
            in
            countWrongPlaceRec
                (acc + List.length secret - List.length secretLeft)
                (Secret secretLeft)
                (Key restOfKey)

        [] ->
            acc


removeOnceFrom : List a -> a -> List a
removeOnceFrom list el =
    removeOnceFromRec list el []


removeOnceFromRec : List a -> a -> List a -> List a
removeOnceFromRec end el start =
    case end of
        head :: tail ->
            if head == el then
                start ++ tail

            else
                removeOnceFromRec tail el (start ++ [ head ])

        [] ->
            start
