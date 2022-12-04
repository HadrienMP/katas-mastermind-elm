module Main exposing (Model, Msg, main)

import Browser
import Browser.Navigation as Nav
import Core exposing (Key, Secret)
import Css
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (onClick)
import KeyInput exposing (KeyInput)
import Pin exposing (Pin)
import Random
import Random.List
import UI.Pin
import UI.Result
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
    , results : List ( Key, Core.Result )
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
      , input = KeyInput.empty size
      , secret = Core.Secret []
      , selected = Nothing
      , cheat = False
      }
    , (Random.List.choose Pin.all
        |> Random.map Tuple.first
        |> Random.map (Maybe.withDefault Pin.Red)
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
        |> Random.map Core.Secret
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
                | input = KeyInput.put { position = position, pin = model.selected } model.input
              }
            , Cmd.none
            )

        Cheat ->
            ( { model | cheat = not model.cheat }, Cmd.none )

        Check key ->
            ( { model
                | results =
                    ( key, Core.score key model.secret )
                        :: model.results
                , selected = Nothing
                , input = KeyInput.clear model.input
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
                    , Css.margin2 (Css.rem 3) Css.zero
                    , Css.border3 (Css.px 2) Css.solid <| Css.hex "#000"
                    , Css.fontFamily Css.sansSerif
                    , Css.displayFlex
                    , Css.flexDirection Css.column
                    , defaultGap
                    , Css.position Css.absolute
                    , Css.left <| Css.vw 50
                    , Css.transform <| Css.translateX (Css.pct -50)
                    ]
                ]
                [ title
                , Html.div [ onClick Cheat ]
                    [ if model.cheat then
                        case model.secret of
                            Core.Secret secret ->
                                Html.div [ Attr.css [ Css.displayFlex, defaultGap ] ]
                                    [ secret |> List.map UI.Pin.display |> pinBox
                                    , Html.button
                                        [ Attr.css [ Css.height <| Css.rem 2 ]
                                        ]
                                        [ Html.text "Hide" ]
                                    ]

                      else
                        Html.button
                            [ Attr.css [ Css.width <| Css.pct 100, Css.height <| Css.rem 2 ]
                            ]
                            [ Html.text "Cheat" ]
                    ]
                , verticalSeparator
                , Pin.all
                    |> List.map (actionPin model.selected)
                    |> pinBox
                , viewInput model
                , verticalSeparator
                , Html.div
                    [ Attr.css
                        [ Css.displayFlex
                        , Css.flexDirection Css.column
                        , defaultGap
                        ]
                    ]
                    (model.results |> List.map viewResult)
                ]
        ]
    }


title : Html.Html msg
title =
    Html.h1
        [ Attr.css
            [ Css.textTransform Css.uppercase
            , Css.margin Css.zero
            , Css.fontSize <| Css.rem 2.5
            , Css.textAlign Css.center
            ]
        ]
        [ Html.text "Mastermind" ]


verticalSeparator : Html.Html msg
verticalSeparator =
    Html.hr [ Attr.css [ Css.width <| Css.pct 100 ] ] []


viewInput : Model -> Html.Html Msg
viewInput model =
    Html.div
        [ Attr.css
            [ Css.displayFlex
            , defaultGap
            ]
        ]
        [ KeyInput.pins model.input
            |> List.map selectionPin
            |> pinBox
        , Html.button
            (case KeyInput.parse model.input of
                Just key ->
                    [ onClick <| Check key ]

                Nothing ->
                    [ Attr.disabled True ]
            )
            [ Html.text "Check" ]
        ]


selectionPin : ( Int, Maybe Pin ) -> Html.Html Msg
selectionPin ( position, pin ) =
    actionablePinSlot position <|
        case pin of
            Just it ->
                UI.Pin.display it

            Nothing ->
                UI.Pin.slot


actionPin : Maybe Pin -> Pin -> Html.Html Msg
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


viewResult : ( Key, Core.Result ) -> Html.Html Msg
viewResult ( Core.Key key, { rightPlace, wrongPlace } ) =
    Html.div
        [ Attr.css
            [ Css.displayFlex
            , defaultGap
            , Css.alignItems Css.center
            ]
        ]
        [ key
            |> List.map UI.Pin.display
            |> pinBox
        , Html.div [ Attr.css [ Css.displayFlex, cssGap <| Rem 0.2 ] ]
            (List.repeat rightPlace UI.Result.rightPlace
                ++ List.repeat wrongPlace UI.Result.wrongPlace
            )
        ]


pinBox : List (Html.Html Msg) -> Html.Html Msg
pinBox =
    Html.div
        [ Attr.css
            [ Css.displayFlex
            , defaultGap
            ]
        ]


defaultGap : Css.Style
defaultGap =
    cssGap <| Rem 1


type Rem
    = Rem Float


cssGap : Rem -> Css.Style
cssGap (Rem it) =
    Css.property "gap" <| String.fromFloat it ++ "rem"


actionablePinSlot : Int -> Html.Html Msg -> Html.Html Msg
actionablePinSlot position child =
    Html.div [ onClick <| PlacedAt position ] [ child ]
