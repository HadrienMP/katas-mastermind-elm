module Domain.Core exposing
    ( Result
    , countRightPlace
    , countWrongPlace
    , score
    )

import Domain.Key exposing (Key(..))
import Domain.Secret exposing (Secret)


type alias Result =
    { rightPlace : Int, wrongPlace : Int }


score : Key -> Secret -> Result
score key secret =
    let
        { count, unmatched } =
            countRightPlace secret key
    in
    { rightPlace = count
    , wrongPlace = countWrongPlace unmatched.secret unmatched.key
    }



-- Wrong place


countWrongPlace : Secret -> Key -> Int
countWrongPlace secret key =
    countWrongPlaceRec 0 secret key


countWrongPlaceRec : Int -> Secret -> Key -> Int
countWrongPlaceRec acc (Domain.Secret.Secret secret) (Key guess) =
    case guess of
        firstKeyPin :: restOfKey ->
            let
                secretLeft =
                    firstKeyPin |> removeOnceFrom secret
            in
            countWrongPlaceRec
                (acc + List.length secret - List.length secretLeft)
                (Domain.Secret.Secret secretLeft)
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


type alias Unmatched =
    { secret : Secret, key : Key }


type alias RightPlaceResult =
    { count : Int, unmatched : Unmatched }


countRightPlace : Secret -> Key -> RightPlaceResult
countRightPlace secret key =
    countRightPlaceRec key
        secret
        { count = 0
        , unmatched =
            { secret = Domain.Secret.empty
            , key = Key []
            }
        }


countRightPlaceRec :
    Key
    -> Secret
    -> RightPlaceResult
    -> RightPlaceResult
countRightPlaceRec (Key key) (Domain.Secret.Secret secret) acc =
    case ( key, secret ) of
        ( keyPin :: restOfKey, secretPin :: restOfSecret ) ->
            countRightPlaceRec
                (Key restOfKey)
                (Domain.Secret.Secret restOfSecret)
                (if keyPin == secretPin then
                    { acc | count = acc.count + 1 }

                 else
                    let
                        unmatchedKeyPins =
                            case acc.unmatched.key of
                                Key it ->
                                    it
                    in
                    { acc
                        | unmatched =
                            { secret = Domain.Secret.append secretPin acc.unmatched.secret
                            , key = Key <| keyPin :: unmatchedKeyPins
                            }
                    }
                )

        _ ->
            acc
