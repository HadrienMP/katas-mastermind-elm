module MastermindSpec exposing (suite)

import Domain.Code exposing (Key, Secret)
import Domain.Core exposing (..)
import Domain.Pin exposing (..)
import Expect
import Test exposing (Test, describe, test)


blackWhitePurple : Secret
blackWhitePurple =
    secret [ Black, White, Purple ]


secret : List Pin -> Secret
secret pins =
    List.map SecretPin pins


key : List Pin -> Key
key pins =
    List.map KeyPin pins


suite : Test
suite =
    describe "mastermind"
        [ describe "score"
            [ test "no match" <|
                \_ ->
                    key [ Blue, Red, Green ]
                        |> score blackWhitePurple
                        |> Expect.equal { rightPlace = 0, wrongPlace = 0 }
            , test "exact match" <|
                \_ ->
                    key [ Blue, Red, Green ]
                        |> score (secret [ Blue, Red, Green ])
                        |> Expect.equal { rightPlace = 3, wrongPlace = 0 }
            , test "one pin off" <|
                \_ ->
                    key [ Blue, Red, Green ]
                        |> score (secret [ Blue, Red, Purple ])
                        |> Expect.equal { rightPlace = 2, wrongPlace = 0 }
            , test "one pin at the wrong place" <|
                \_ ->
                    key [ Purple, Red, Green ]
                        |> score (secret [ Black, White, Purple ])
                        |> Expect.equal { rightPlace = 0, wrongPlace = 1 }
            , test "when the key contains two pins of the same colour and one is in the right place we count only a right place" <|
                \_ ->
                    key [ Blue, Blue, Green ]
                        |> score (secret [ Blue, Red, Purple ])
                        |> Expect.equal { rightPlace = 1, wrongPlace = 0 }
            , test "Key pins already matched don't count as in the wrong place" <|
                \_ ->
                    key [ Red, Red, Blue, Blue ]
                        |> score (secret [ Red, Red, Red, Black ])
                        |> Expect.equal { rightPlace = 2, wrongPlace = 0 }
            ]
        , describe "count right place"
            [ describe "Single pin"
                [ test "no match" <|
                    \_ ->
                        key [ Blue ]
                            |> score (secret [ Red ])
                            |> Expect.equal { rightPlace = 0, wrongPlace = 0 }
                , test "exact match" <|
                    \_ ->
                        key [ Red ]
                            |> score (secret [ Red ])
                            |> Expect.equal { rightPlace = 1, wrongPlace = 0 }
                ]
            , describe "two pins"
                [ test "one match" <|
                    \_ ->
                        key [ Blue, Blue ]
                            |> score (secret [ Blue, Red ])
                            |> Expect.equal { rightPlace = 1, wrongPlace = 0 }
                , test "exact match" <|
                    \_ ->
                        key [ Blue, Blue ]
                            |> score (secret [ Blue, Blue ])
                            |> Expect.equal { rightPlace = 2, wrongPlace = 0 }
                ]
            ]
        , describe "count wrong place"
            [ test "no pin at the wrong place" <|
                \_ ->
                    key [ Red, Blue, Green ]
                        |> score blackWhitePurple
                        |> .wrongPlace
                        |> Expect.equal 0
            , describe "First pin at the wrong place"
                [ test "Purple" <|
                    \_ ->
                        key [ Purple, Blue, Green ]
                            |> score blackWhitePurple
                            |> Expect.equal { rightPlace = 0, wrongPlace = 1 }
                , test "White" <|
                    \_ ->
                        key [ White, Blue, Green ]
                            |> score blackWhitePurple
                            |> .wrongPlace
                            |> Expect.equal 1
                ]
            , describe "Second pin at the wrong place"
                [ test "Purple" <|
                    \_ ->
                        key [ Blue, Purple, Green ]
                            |> score blackWhitePurple
                            |> .wrongPlace
                            |> Expect.equal 1
                , test "White" <|
                    \_ ->
                        key [ White, Blue, Green ]
                            |> score blackWhitePurple
                            |> .wrongPlace
                            |> Expect.equal 1
                ]
            , describe "Two pins at the wrong place"
                [ test "Purple and White" <|
                    \_ ->
                        key [ Purple, Blue, White ]
                            |> score blackWhitePurple
                            |> .wrongPlace
                            |> Expect.equal 2
                ]
            , test
                ("A key pin appearing twice at the wrong place in the key "
                    ++ "but only once in the secret "
                    ++ "is counted at the wrong place only once"
                )
              <|
                \_ ->
                    key [ Purple, Purple, Green ]
                        |> score (secret [ Black, White, Purple ])
                        |> .wrongPlace
                        |> Expect.equal 1
            , test "A key pin appearing twice at the wrong place is counted twice" <|
                \_ ->
                    key [ Purple, Purple, Green, Green ]
                        |> score (secret [ White, White, Purple, Purple ])
                        |> .wrongPlace
                        |> Expect.equal 2
            , test "A key pin appearing twice in the secret counts once" <|
                \_ ->
                    key [ Purple, Green, Green, Green ]
                        |> score (secret [ White, White, Purple, Purple ])
                        |> .wrongPlace
                        |> Expect.equal 1
            ]
        ]
