module MastermindSpec exposing (rightPlaceSuite, suite, wrongPlaceSuite)

import Core exposing (..)
import Expect
import Pin exposing (..)
import Test exposing (Test, describe, test)


blackWhitePurple : Secret
blackWhitePurple =
    Secret [ Black, White, Purple ]


suite : Test
suite =
    describe "score"
        [ test "no match" <|
            \_ ->
                score (Key [ Blue, Red, Green ]) blackWhitePurple
                    |> Expect.equal { rightPlace = 0, wrongPlace = 0 }
        , test "exact match" <|
            \_ ->
                score
                    (Key [ Blue, Red, Green ])
                    (Secret [ Blue, Red, Green ])
                    |> Expect.equal { rightPlace = 3, wrongPlace = 0 }
        , test "one pin off" <|
            \_ ->
                score
                    (Key [ Blue, Red, Green ])
                    (Secret [ Blue, Red, Purple ])
                    |> Expect.equal { rightPlace = 2, wrongPlace = 0 }
        , test "one pin at the wrong place" <|
            \_ ->
                score
                    (Key [ Purple, Red, Green ])
                    (Secret [ Black, White, Purple ])
                    |> Expect.equal { rightPlace = 0, wrongPlace = 1 }
        , test "when the key contains two pins of the same colour and one is in the right place we count only a right place" <|
            \_ ->
                score
                    (Key [ Blue, Blue, Green ])
                    (Secret [ Blue, Red, Purple ])
                    |> Expect.equal { rightPlace = 1, wrongPlace = 0 }
        ]


rightPlaceSuite : Test
rightPlaceSuite =
    describe "count right place"
        [ describe "Single pin"
            [ test "no match" <|
                \_ ->
                    Key [ Blue ]
                        |> countRightPlace (Secret [ Red ])
                        |> .count
                        |> Expect.equal 0
            , test "exact match" <|
                \_ ->
                    Key [ Red ]
                        |> countRightPlace (Secret [ Red ])
                        |> .count
                        |> Expect.equal 1
            ]
        , describe "two pins"
            [ test "one match" <|
                \_ ->
                    Key [ Blue, Blue ]
                        |> countRightPlace (Secret [ Blue, Red ])
                        |> .count
                        |> Expect.equal 1
            , test "exact match" <|
                \_ ->
                    Key [ Blue, Blue ]
                        |> countRightPlace (Secret [ Blue, Blue ])
                        |> .count
                        |> Expect.equal 2
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



-- TODO: a pin appearing twice in the secret and wrong place one