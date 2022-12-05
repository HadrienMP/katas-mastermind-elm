module MastermindSpec exposing (rightPlaceSuite, suite, wrongPlaceSuite)

import Domain.Core exposing (..)
import Domain.Key exposing (Key(..))
import Domain.Pin exposing (..)
import Domain.Secret exposing (Secret(..))
import Expect
import Test exposing (Test, describe, test)


blackWhitePurple : Secret
blackWhitePurple =
    Secret [ Black, White, Purple ]


suite : Test
suite =
    describe "score"
        [ test "no match" <|
            \_ ->
                Key [ Blue, Red, Green ]
                    |> score blackWhitePurple
                    |> Expect.equal { rightPlace = 0, wrongPlace = 0 }
        , test "exact match" <|
            \_ ->
                Key [ Blue, Red, Green ]
                    |> score (Secret [ Blue, Red, Green ])
                    |> Expect.equal { rightPlace = 3, wrongPlace = 0 }
        , test "one pin off" <|
            \_ ->
                Key [ Blue, Red, Green ]
                    |> score (Secret [ Blue, Red, Purple ])
                    |> Expect.equal { rightPlace = 2, wrongPlace = 0 }
        , test "one pin at the wrong place" <|
            \_ ->
                Key [ Purple, Red, Green ]
                    |> score (Secret [ Black, White, Purple ])
                    |> Expect.equal { rightPlace = 0, wrongPlace = 1 }
        , test "when the key contains two pins of the same colour and one is in the right place we count only a right place" <|
            \_ ->
                Key [ Blue, Blue, Green ]
                    |> score (Secret [ Blue, Red, Purple ])
                    |> Expect.equal { rightPlace = 1, wrongPlace = 0 }
        , test "Key pins already matched don't count as in the wrong place" <|
            \_ ->
                Key [ Red, Red, Blue, Blue ]
                    |> score (Secret [ Red, Red, Red, Black ])
                    |> Expect.equal { rightPlace = 2, wrongPlace = 0 }
        ]


rightPlaceSuite : Test
rightPlaceSuite =
    describe "count right place"
        [ describe "Single pin"
            [ test "no match" <|
                \_ ->
                    Key [ Blue ]
                        |> score (Secret [ Red ])
                        |> Expect.equal { rightPlace = 0, wrongPlace = 0 }
            , test "exact match" <|
                \_ ->
                    Key [ Red ]
                        |> score (Secret [ Red ])
                        |> Expect.equal { rightPlace = 1, wrongPlace = 0 }
            ]
        , describe "two pins"
            [ test "one match" <|
                \_ ->
                    Key [ Blue, Blue ]
                        |> score (Secret [ Blue, Red ])
                        |> Expect.equal { rightPlace = 1, wrongPlace = 0 }
            , test "exact match" <|
                \_ ->
                    Key [ Blue, Blue ]
                        |> score (Secret [ Blue, Blue ])
                        |> Expect.equal { rightPlace = 2, wrongPlace = 0 }
            ]
        ]



-- TODO: a pin that is in the right place cannot be counted in the wrong place


wrongPlaceSuite : Test
wrongPlaceSuite =
    describe "count wrong place"
        [ test "no pin at the wrong place" <|
            \_ ->
                Key [ Red, Blue, Green ]
                    |> countWrongPlace blackWhitePurple
                    |> Expect.equal 0
        , describe "First pin at the wrong place"
            [ test "Purple" <|
                \_ ->
                    Key [ Purple, Blue, Green ]
                        |> countWrongPlace blackWhitePurple
                        |> Expect.equal 1
            , test "White" <|
                \_ ->
                    Key [ White, Blue, Green ]
                        |> countWrongPlace blackWhitePurple
                        |> Expect.equal 1
            ]
        , describe "Second pin at the wrong place"
            [ test "Purple" <|
                \_ ->
                    Key [ Blue, Purple, Green ]
                        |> countWrongPlace blackWhitePurple
                        |> Expect.equal 1
            , test "White" <|
                \_ ->
                    Key [ Blue, White, Green ]
                        |> countWrongPlace blackWhitePurple
                        |> Expect.equal 1
            ]
        , describe "Two pins at the wrong place"
            [ test "Purple and White" <|
                \_ ->
                    Key [ Purple, Blue, White ]
                        |> countWrongPlace blackWhitePurple
                        |> Expect.equal 2
            ]
        , test
            ("A key pin appearing twice at the wrong place in the key "
                ++ "but only once in the secret "
                ++ "is counted at the wrong place only once"
            )
          <|
            \_ ->
                Key [ Purple, Purple, Green ]
                    |> countWrongPlace (Secret [ Black, White, Purple ])
                    |> Expect.equal 1
        , test "A key pin appearing twice at the wrong place is counted twice" <|
            \_ ->
                Key [ Purple, Purple, Green, Green ]
                    |> countWrongPlace (Secret [ White, White, Purple, Purple ])
                    |> Expect.equal 2
        , test "A key pin appearing twice in the secret counts once" <|
            \_ ->
                Key [ Purple, Green, Green, Green ]
                    |> countWrongPlace (Secret [ White, White, Purple, Purple ])
                    |> Expect.equal 1
        ]
