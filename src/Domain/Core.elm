module Domain.Core exposing (score)

import AssocList as Dict
import Domain.Code exposing (Key, Secret)
import Domain.Pin exposing (KeyPin(..), Pin, SecretPin(..))
import Maybe



-- Accumulator


type alias Count =
    { rightPlace : Int, wrongPlace : CountByPin }


emptyCount : Count
emptyCount =
    { rightPlace = 0, wrongPlace = Dict.empty }


type alias CountByPin =
    Dict.Dict Pin CountByCode


type alias CountByCode =
    { key : Int, secret : Int }


incrementKey : CountByCode -> CountByCode
incrementKey it =
    { it | key = it.key + 1 }


incrementSecret : CountByCode -> CountByCode
incrementSecret it =
    { it | secret = it.secret + 1 }



-- Score


score : Secret -> Key -> { rightPlace : Int, wrongPlace : Int }
score secret key =
    countMatches emptyCount secret key
        |> consolidateWrongPlaces


countMatches : Count -> Secret -> Key -> Count
countMatches count secret key =
    case ( secret, key ) of
        ( hs :: ts, hk :: tk ) ->
            countMatches
                (addToCount ( hk, hs ) count)
                ts
                tk

        _ ->
            count


addToCount : ( KeyPin, SecretPin ) -> Count -> Count
addToCount ( keyPin, secretPin ) acc =
    if Domain.Pin.equal keyPin secretPin then
        { acc | rightPlace = acc.rightPlace + 1 }

    else
        { acc | wrongPlace = addToWrongPlaces ( keyPin, secretPin ) acc.wrongPlace }


addToWrongPlaces : ( KeyPin, SecretPin ) -> CountByPin -> CountByPin
addToWrongPlaces ( KeyPin key, SecretPin secret ) wrongPlaces =
    wrongPlaces
        |> Dict.update key (updateCountBy incrementKey)
        |> Dict.update secret (updateCountBy incrementSecret)


updateCountBy : (CountByCode -> CountByCode) -> Maybe CountByCode -> Maybe CountByCode
updateCountBy increment maybeCount =
    maybeCount
        |> Maybe.withDefault { key = 0, secret = 0 }
        |> increment
        |> Just


consolidateWrongPlaces : Count -> { rightPlace : Int, wrongPlace : Int }
consolidateWrongPlaces acc =
    { rightPlace = acc.rightPlace
    , wrongPlace =
        acc.wrongPlace
            |> Dict.values
            |> List.map (\{ key, secret } -> min key secret)
            |> List.sum
    }
