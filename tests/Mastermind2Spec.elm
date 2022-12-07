module Mastermind2Spec exposing (..)

import Domain.Pin exposing (KeyPin(..), Pin(..), SecretPin(..))
import Expect
import List.Extra
import Test exposing (Test, describe, test)


type alias Row =
    List ( KeyPin, SecretPin )


makeSecret : List Pin -> List SecretPin
makeSecret =
    List.map SecretPin


weaveKey : List Pin -> List SecretPin -> Row
weaveKey key secret =
    List.Extra.zip (List.map KeyPin key) secret


suite : Test
suite =
    describe "Mastermind 2"
        [ test "no match" <|
            \_ ->
                blackWhitePurpleSecret
                    |> weaveKey [ Red, Green, Blue ]
                    |> scoreRow
                    |> Expect.equal { right = 0, wrong = 0 }
        , describe "Exact matches"
            [ test "exact match in first position" <|
                \_ ->
                    blackWhitePurpleSecret
                        |> weaveKey [ Black, Green, Blue ]
                        |> scoreRow
                        |> Expect.equal { right = 1, wrong = 0 }
            , test "exact match in second position" <|
                \_ ->
                    blackWhitePurpleSecret
                        |> weaveKey [ Red, White, Blue ]
                        |> scoreRow
                        |> Expect.equal { right = 1, wrong = 0 }
            , test "exact match for second and third position" <|
                \_ ->
                    blackWhitePurpleSecret
                        |> weaveKey [ Red, White, Purple ]
                        |> scoreRow
                        |> Expect.equal { right = 2, wrong = 0 }
            ]
        , describe "Wrong Places"
            [ test "wrong color in first place" <|
                \_ ->
                    blackWhitePurpleSecret
                        |> weaveKey [ White, Green, Blue ]
                        |> scoreRow
                        |> Expect.equal { right = 0, wrong = 1 }
            , test "wrong color in the two first places" <|
                \_ ->
                    blackWhitePurpleSecret
                        |> weaveKey [ White, Purple, Blue ]
                        |> scoreRow
                        |> Expect.equal { right = 0, wrong = 2 }
            , test "wrong color in the last place" <|
                \_ ->
                    blackWhitePurpleSecret
                        |> weaveKey [ Red, Blue, Black ]
                        |> scoreRow
                        |> Expect.equal { right = 0, wrong = 1 }
            ]
        , describe "Exact and wrong"
            [ test "exact in first, wrong in 2" <|
                \_ ->
                    blackWhitePurpleSecret
                        |> weaveKey [ Black, Purple, Blue ]
                        |> scoreRow
                        |> Expect.equal { right = 1, wrong = 1 }
            , test "wrong in first, exact in 2" <|
                \_ ->
                    blackWhitePurpleSecret
                        |> weaveKey [ Purple, White, Blue ]
                        |> scoreRow
                        |> Expect.equal { right = 1, wrong = 1 }
            ]
        , describe "Double colors"
            [ test "the colors has an exact match and a wrong place" <|
                \_ ->
                    makeSecret [ Black, White, Black ]
                        |> weaveKey [ Black, Black, Blue ]
                        |> scoreRow
                        |> Expect.equal { right = 1, wrong = 1 }
            , test "double in key, single in secret" <|
                \_ ->
                    blackWhitePurpleSecret
                        |> weaveKey [ Black, Black, Blue ]
                        |> scoreRow
                        |> Expect.equal { right = 1, wrong = 0 }
            , test "double in key, single in secret, exact match in second place" <|
                \_ ->
                    blackWhitePurpleSecret
                        |> weaveKey [ White, White, Blue ]
                        |> scoreRow
                        |> Expect.equal { right = 1, wrong = 0 }
            , test "wrong place already counted" <|
                \_ ->
                    blackWhitePurpleSecret
                        |> weaveKey [ White, Blue, White ]
                        |> scoreRow
                        |> Expect.equal { right = 0, wrong = 1 }
            ]
        ]


blackWhitePurpleSecret : List SecretPin
blackWhitePurpleSecret =
    makeSecret [ Black, White, Purple ]



-- Score


type alias Score =
    { right : Int, wrong : Int }


incrementRight : Score -> Score
incrementRight score =
    { score | right = score.right + 1 }


incrementWrong : Score -> Score
incrementWrong score =
    { score | wrong = score.wrong + 1 }



-- Score Row


scoreRow : Row -> Score
scoreRow row =
    row
        |> countRightPlace
        |> countWrongPlace
        |> .score



-- Right Place


type alias RightPlaceContext =
    { score : Score, mismatch : Row }


countRightPlace : Row -> RightPlaceContext
countRightPlace row =
    rightPlaceRec row initCtx


rightPlaceRec : Row -> RightPlaceContext -> RightPlaceContext
rightPlaceRec toAnalyse ctx =
    case toAnalyse of
        ( KeyPin key, SecretPin secret ) :: rest ->
            rightPlaceRec rest
                (if key == secret then
                    { ctx | score = incrementRight ctx.score }

                 else
                    { ctx | mismatch = ( KeyPin key, SecretPin secret ) :: ctx.mismatch }
                )

        [] ->
            ctx


initCtx : RightPlaceContext
initCtx =
    { score = { right = 0, wrong = 0 }, mismatch = [] }



-- Wong Place


type alias WrongPlaceContext =
    { score : Score, mismatch : List SecretPin }


countWrongPlace : RightPlaceContext -> WrongPlaceContext
countWrongPlace rightPlaceCtx =
    let
        ( key, secret ) =
            List.unzip rightPlaceCtx.mismatch
    in
    wrongPlaceRec key { score = rightPlaceCtx.score, mismatch = secret }


wrongPlaceRec : List KeyPin -> WrongPlaceContext -> WrongPlaceContext
wrongPlaceRec toAnalyse ctx =
    case toAnalyse of
        (KeyPin key) :: rest ->
            wrongPlaceRec rest
                (case removeOnce (SecretPin key) ctx.mismatch of
                    ( Just _, mismatchLeft ) ->
                        { ctx
                            | score = incrementWrong ctx.score
                            , mismatch = mismatchLeft
                        }

                    ( Nothing, _ ) ->
                        ctx
                )

        [] ->
            ctx


removeOnce : a -> List a -> ( Maybe a, List a )
removeOnce target list =
    removeOnceRec target { start = [], end = list }


removeOnceRec : a -> { start : List a, end : List a } -> ( Maybe a, List a )
removeOnceRec target ctx =
    case ctx.end of
        head :: tail ->
            if head == target then
                ( Just target, ctx.start ++ tail )

            else
                removeOnceRec target { start = head :: ctx.start, end = tail }

        [] ->
            ( Nothing, ctx.start ++ ctx.end )
