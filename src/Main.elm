module Main exposing (Model, Msg, main)

import Browser
import Browser.Navigation as Nav
import Css
import Domain.Code exposing (Key, Secret)
import Domain.Core
import Domain.Pin exposing (Pin, SecretPin(..))
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (onClick)
import Random
import Random.List
import UI.Flex
import UI.KeyInput exposing (KeyInput)
import UI.Pin
import UI.Result
import UI.Typography
import Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


type alias Model =
    { navkey : Nav.Key
    , url : Url.Url
    , results : List ( Key, { rightPlace : Int, wrongPlace : Int } )
    , input : KeyInput
    , secret : Secret
    , selected : Maybe Pin
    , cheat : Bool
    }


size : Int
size =
    4


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { navkey = key
      , url = url
      , results = []
      , input = UI.KeyInput.empty size
      , secret = []
      , selected = Nothing
      , cheat = False
      }
    , (Random.List.choose Domain.Pin.all
        |> Random.map Tuple.first
        |> Random.map (Maybe.withDefault Domain.Pin.Red)
        |> Random.map SecretPin
      )
        |> List.repeat size
        |> List.foldl
            (\pinGenerator listGenerator ->
                Random.andThen
                    (\list ->
                        pinGenerator
                            |> Random.map (\pin -> pin :: list)
                    )
                    listGenerator
            )
            (Random.constant [])
        |> Random.generate SecretGenerated
    )


type Msg
    = PlacedAt Int
    | Selected Pin
    | Check Key
    | Cheat
    | SecretGenerated Secret
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SecretGenerated secret ->
            ( { model | secret = secret }, Cmd.none )

        Selected pin ->
            ( { model | selected = Just pin }, Cmd.none )

        PlacedAt position ->
            ( { model
                | input = UI.KeyInput.put { position = position, pin = model.selected } model.input
              }
            , Cmd.none
            )

        Cheat ->
            ( { model | cheat = not model.cheat }, Cmd.none )

        Check key ->
            ( { model
                | results =
                    ( key, Domain.Core.score model.secret key )
                        :: model.results
                , selected = Nothing
                , input = UI.KeyInput.clear model.input
              }
            , Cmd.none
            )

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navkey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    { title = "Mastermind"
    , body =
        [ Html.toUnstyled <|
            Html.div
                [ Attr.css
                    [ Css.padding <| Css.rem 1
                    , Css.margin2 (Css.rem 3) Css.auto
                    , Css.border3 (Css.px 2) Css.solid <| Css.hex "#000"
                    , Css.fontFamily Css.sansSerif
                    , Css.displayFlex
                    , Css.flexDirection Css.column
                    , UI.Flex.defaultGap
                    , Css.maxWidth Css.fitContent
                    ]
                ]
                [ UI.Typography.title "Mastermind"
                , secretView model
                , verticalSeparator
                , Domain.Pin.all
                    |> List.map (actionPin model.selected)
                    |> UI.Pin.pinBox
                , UI.KeyInput.view
                    { onCheck = Check
                    , onSlotClick = PlacedAt
                    }
                    model.input
                , verticalSeparator
                , Html.div
                    [ Attr.css
                        [ Css.displayFlex
                        , Css.flexDirection Css.column
                        , UI.Flex.defaultGap
                        ]
                    ]
                    (model.results |> List.map viewResult)
                ]
        ]
    }


secretView : Model -> Html Msg
secretView model =
    Html.div [ onClick Cheat ]
        [ if model.cheat then
            Html.div
                [ Attr.css
                    [ Css.displayFlex
                    , UI.Flex.defaultGap
                    ]
                ]
                [ model.secret
                    |> List.map Domain.Pin.openSecret
                    |> List.map UI.Pin.display
                    |> UI.Pin.pinBox
                , Html.button
                    [ Attr.css [ Css.height <| Css.rem 2 ] ]
                    [ Html.text "Hide" ]
                ]

          else
            Html.button
                [ Attr.css
                    [ Css.width <| Css.pct 100
                    , Css.height <| Css.rem 2
                    ]
                ]
                [ Html.text "Cheat" ]
        ]


verticalSeparator : Html msg
verticalSeparator =
    Html.hr [ Attr.css [ Css.width <| Css.pct 100 ] ] []


actionPin : Maybe Pin -> Pin -> Html Msg
actionPin selected current =
    Html.div
        [ Attr.class "action-pin"
        , onClick <| Selected current
        , Attr.css
            [ Css.transform <|
                Css.scale <|
                    if selected == Just current then
                        1.2

                    else
                        1
            ]
        ]
        [ UI.Pin.display current ]


viewResult : ( Key, { rightPlace : Int, wrongPlace : Int } ) -> Html Msg
viewResult ( key, { rightPlace, wrongPlace } ) =
    Html.div
        [ Attr.css
            [ Css.displayFlex
            , UI.Flex.defaultGap
            , Css.alignItems Css.center
            ]
        ]
        [ key
            |> List.map Domain.Pin.openKey
            |> List.map UI.Pin.display
            |> UI.Pin.pinBox
        , Html.div [ Attr.css [ Css.displayFlex, UI.Flex.cssGap <| UI.Flex.Rem 0.2 ] ]
            (List.repeat rightPlace UI.Result.rightPlace
                ++ List.repeat wrongPlace UI.Result.wrongPlace
            )
        ]
