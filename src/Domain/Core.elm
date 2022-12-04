module Domain.Core exposing
    ( Key(..)
    , Result
    , Secret(..)
    , countRightPlace
    , countWrongPlace
    , score
    )

import Domain.Pin exposing (Pin)


type alias Result =
    { rightPlace : Int, wrongPlace : Int }


type Key
    = Key (List Pin)


type Secret
    = Secret (List Pin)


appendToSecret : Pin -> Secret -> Secret
appendToSecret pin (Secret secret) =
    Secret <| secret ++ [ pin ]


score : Key -> Secret -> Result
score key secret =
    let
        { count, unmatched } =
            countRightPlace secret key
    in
    { rightPlace = count
    , wrongPlace = countWrongPlace unmatched key
    }



-- Wrong place


countWrongPlace : Secret -> Key -> Int
countWrongPlace secret key =
    key
        |> countWrongPlaceRec 0 secret


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



-- Right place


countRightPlace : Secret -> Key -> { count : Int, unmatched : Secret }
countRightPlace secret key =
    countRightPlaceRec key secret { count = 0, unmatched = Secret [] }


countRightPlaceRec :
    Key
    -> Secret
    -> { count : Int, unmatched : Secret }
    -> { count : Int, unmatched : Secret }
countRightPlaceRec (Key key) (Secret secret) acc =
    case ( key, secret ) of
        ( keyPin :: restOfKey, secretPin :: restOfSecret ) ->
            countRightPlaceRec
                (Key restOfKey)
                (Secret restOfSecret)
                (if keyPin == secretPin then
                    { acc | count = acc.count + 1 }

                 else
                    { acc
                        | unmatched = appendToSecret secretPin acc.unmatched
                    }
                )

        _ ->
            acc
